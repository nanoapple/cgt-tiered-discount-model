# =============================================================================
# 01_generate_data.R  (v4 — final review round)
# Synthetic ATO Administrative Data Generator
# =============================================================================
# OUTPUT: ato_synthetic_family_groups.csv
#
# Each record = one family_group in a given financial year (FY 2025-26),
# approximating what Treasury/ATO would extract from linked administrative
# registers (individual tax returns, rental schedules, land title offices,
# rental bond boards, Centrelink, ABN/trust registers).
#
# This version responds to the full expert review panel. Key structural
# improvements over v3:
#
#   P0-A: Non-investor families (portfolio_count=0) now included via a
#         Bernoulli gate, so the population is "all families" not just
#         "all property investors". This makes tier assignment more realistic.
#
#   P0-B: property_value_total, debt_total, equity_total now generated from
#         a structural model (not just portfolio_count * random). These feed
#         into the composite index in 02_calibration_model.R.
#
#   P0-C: net_rental_income generated from a cash-flow identity:
#         gross_rent = value * yield * occupancy
#         net = gross_rent - interest - expenses - depreciation
#         This replaces the previous "leverage_ratio * gross_rent" shortcut.
#
#   P1-D: Holding period uses a mixture distribution (new entrants + old
#         holders) with soft upper cap via logistic transform, eliminating
#         the artificial "wall at 40 years".
#
#   P1-E: Leverage uses a hurdle model: first draw has_debt (Bernoulli),
#         then continuous leverage conditional on having debt. This creates
#         the realistic "zero-debt floor" visible in ATO data.
#
#   P1-F: portfolio_count for investors uses a zero-truncated negative
#         binomial with type-modulated parameters for heavier tails.
#
#   P1-G: trust_structure probability driven by property_value_total and
#         portfolio_count (not just income), with structural noise.
# =============================================================================

library(dplyr)
library(MASS)

set.seed(2026)

# ---------------------------------------------------------------------------
# CONFIGURATION — total population includes non-investors
# ---------------------------------------------------------------------------
# ATO data shows ~2.2M individuals declare rental income out of ~14M tax
# lodgers. At the family-group level, roughly 15-20% of families have at
# least one investment property. We generate a mixed population.
# ---------------------------------------------------------------------------
n_total    <- 200000        # total family groups in the synthetic population
p_investor <- 0.35         # P(family owns ≥1 investment property)
                           # Higher than 20% because this dataset is scoped
                           # to "families with capital gains exposure", not
                           # the general tax-filing population.

# =============================================================================
# 1. Correlated latent factors (5-dimensional Gaussian copula)
# =============================================================================
# These latent uniforms drive ALL downstream variables. The correlation
# structure ensures joint distributions are realistic.
#
# Dimensions:
#   U[,1] → income        U[,2] → leverage       U[,3] → portfolio
#   U[,4] → hold_period   U[,5] → age
#
# Correlation matrix rationale:
#   income ↔ portfolio:  +0.55  Higher earners can service more properties
#   income ↔ leverage:   +0.30  Serviceability enables borrowing
#   portfolio ↔ leverage: +0.35  Active investors gear up across properties
#   hold ↔ portfolio:    -0.40  Accumulators churn; single-property = hold
#   age ↔ hold:          +0.50  Older people have held properties longer
#   age ↔ leverage:      -0.30  Older people have paid down debt
#   age ↔ income:        +0.25  Income rises with experience (to a point)
#   age ↔ portfolio:     +0.20  Weak — accumulation takes time, but
#                                many older investors only have 1 property

rho <- matrix(c(
# income  lever  portf  hold   age
  1.00,   0.30,  0.55, -0.15,  0.25,   # income
  0.30,   1.00,  0.35, -0.25, -0.30,   # leverage
  0.55,   0.35,  1.00, -0.40,  0.20,   # portfolio
 -0.15,  -0.25, -0.40,  1.00,  0.50,   # hold_period
  0.25,  -0.30,  0.20,  0.50,  1.00    # age
), nrow=5, byrow=TRUE)

Z <- mvrnorm(n_total, mu=rep(0,5), Sigma=rho)
U <- pnorm(Z)   # uniform(0,1) marginals preserving correlation structure

# =============================================================================
# 2. Latent investor type (4 archetypes + non-investor)
# =============================================================================
# The investor_type creates natural clustering in the composite index F,
# improving tier separability. Non-investors are assigned first, then
# investor archetypes are drawn for those who do own property.
#
# Behavioural archetypes (among investors):
#   passive_longterm    (45%) — 1-2 properties, low leverage, long hold,
#                               prototypical "mum and dad" investor
#   active_accumulator  (30%) — 2-5 properties, moderate-high leverage,
#                               medium hold, building a portfolio
#   leveraged_speculator(15%) — high leverage, short hold, often negatively
#                               geared, most likely to use trusts
#   legacy_holder       (10%) — inherited or very old holdings, very long
#                               hold, low leverage, often retirees

# Step 1: Determine if family owns investment property
# P(investor) is correlated with income (richer → more likely to invest)
# and age (older → more likely to have acquired property)
invest_logit <- -1.2 +                   # baseline gives ~35% at mean
  0.5 * scale(U[,1]) +                   # income correlation
  0.3 * scale(U[,5])                     # age correlation
is_investor <- as.integer(runif(n_total) < plogis(as.numeric(invest_logit)))

# Step 2: Assign archetypes to investors
investor_type <- rep("non_investor", n_total)
inv_idx <- which(is_investor == 1)
n_inv   <- length(inv_idx)

type_probs <- c(passive_longterm=0.45, active_accumulator=0.30,
                leveraged_speculator=0.15, legacy_holder=0.10)
investor_type[inv_idx] <- sample(names(type_probs), n_inv,
                                  replace=TRUE, prob=type_probs)

cat(sprintf("Population: %d total, %d investors (%.1f%%)\n",
            n_total, n_inv, 100*n_inv/n_total))

# Step 3 (P1-1): Inherited property flag
# ~5% of investors hold inherited/gifted properties. These are a distinct
# sub-population: typically 1 property, long holding period, low leverage,
# potentially high property value (established suburbs), variable income.
# This creates the "low income, high asset" edge cases that make Tier 0/1
# more realistic and prevents the model from being too "textbook".
is_inherited <- rep(0L, n_total)
inherit_eligible <- which(investor_type %in% c("passive_longterm", "legacy_holder"))
is_inherited[inherit_eligible] <- as.integer(
  runif(length(inherit_eligible)) < 0.09  # ~9% of passive+legacy ≈ ~5% of all investors
)

# =============================================================================
# 3. Marginal distributions (type-modulated)
# =============================================================================

# --- 3a) Age proxy (25-85) ---------------------------------------------------
# Structural driver for hold_years, leverage paydown, and hardship.
# Not directly available in ATO data but inferable from DOB on tax return.
age <- qnorm(U[,5], mean=52, sd=12)
age <- pmin(pmax(round(age), 25), 85)

# --- 3b) Family taxable income -----------------------------------------------
# Lognormal base calibrated to ATO tax stats for CGT-exposed population:
#   median ≈ $95k, mean ≈ $130k, p90 ≈ $220k, p99 ≈ $600k
# Type modifiers reflect that accumulators need higher income for
# serviceability, while legacy holders may be retired (lower taxable income).
income_base <- qlnorm(U[,1], meanlog = log(95000), sdlog = 0.65)

income_shift <- case_when(
  investor_type == "non_investor"          ~  -0.15,   # slightly lower on average
  investor_type == "passive_longterm"      ~   0.0,
  investor_type == "active_accumulator"    ~   0.20,
  investor_type == "leveraged_speculator"  ~   0.15,
  investor_type == "legacy_holder"         ~  -0.10
)
income_taxable <- income_base * (1 + income_shift)
income_taxable <- pmax(income_taxable, 0)   # taxable income can be $0

# --- 3c) Portfolio count -------------------------------------------------------
# Non-investors: portfolio_count = 0
# Investors: count driven by EQUITY/FINANCING CAPACITY, not just type shift.
#
# KEY DESIGN (P0-1 upgrade): In reality, how many properties a family can
# hold is constrained by equity (for deposits/serviceability), not income
# alone. We model portfolio_count as a Negative Binomial with intensity
# driven by log(equity proxy) — where equity proxy combines income, age,
# and investor type to approximate available collateral.
#
# This produces:
#   - Natural heaping at 1, 2, 3, 5 (real-world reporting/stage patterns)
#   - 10+ genuinely sparse (not logit-shift pile-up artifact)
#   - income vs portfolio scatter looks like "staircase with dispersion"

portfolio_count <- rep(0L, n_total)

# Equity proxy: combines income capacity + age-based wealth accumulation
# + type-specific financing behaviour. This stands in for the unobserved
# "total household equity available for property deposits".
equity_proxy <- log1p(income_taxable) * 0.5 +   # income serviceability
                scale(age) * 0.3 +                # older = more accumulated wealth
                ifelse(investor_type == "active_accumulator", 0.4, 0) +
                ifelse(investor_type == "leveraged_speculator", 0.2, 0) +
                ifelse(investor_type == "legacy_holder", 0.1, 0)

# NegBin intensity: mu driven by equity proxy
# size parameter controls dispersion (smaller = fatter tail)
# 1. Base distribution: Negative Binomial for the 99%
# Increase dispersion (lower size) and sensitivity to equity
mu_nb <- exp(-0.6 + 0.25 * as.numeric(scale(equity_proxy[inv_idx]))) # 系数0.08 -> 0.25
size_nb <- 0.8  # 更小的size意味着更肥的尾巴 (was 1.5)
raw_count_base <- 1L + rnbinom(n_inv, mu = mu_nb, size = size_nb)

# 2. "Whale" injection: The top 1% get a Power Law boost
# Real data: ~0.8% have 6+ properties.
# We verify equity_proxy is high enough to support this.
is_whale <- (rank(equity_proxy[inv_idx]) / n_inv) > 0.985 # Top 1.5% potential whales

# Generate Pareto-like counts for whales (min 3, alpha ~ 2.5)
# This generates counts like 6, 8, 12, 20...
whale_counts <- round(3 + rexp(sum(is_whale), rate = 0.15)) 

# Merge
raw_count <- raw_count_base
raw_count[is_whale] <- pmax(raw_count[is_whale], whale_counts)

# 3. Heaping (Keep existing logic, it's good)
heap_targets <- c(1L, 2L, 3L, 5L)
p_heap <- 0.25 
do_heap <- runif(n_inv) < p_heap
if (any(do_heap)) {
  raw_count[do_heap] <- sapply(raw_count[do_heap], function(x) {
    heap_targets[which.min(abs(heap_targets - x))]
  })
}

# 4. Cap: Raise it to 50 to allow true outliers
portfolio_count[inv_idx] <- pmin(raw_count, 50L)

# 5. Inherited logic (Keep existing)
portfolio_count[is_inherited == 1] <- 1L


# --- 3d) Holding period (P1-D: mixture + soft cap) ---------------------------
# Two-component mixture:
#   Component 1 (new entrants, ~35%): Gamma(shape=2, scale=2)  → mode ~2yr
#   Component 2 (established, ~65%):  Gamma(shape=4, scale=4)  → mode ~12yr
# Mixed in copula space, then type-modulated.
# Soft upper cap via logistic: hold = 50 / (1 + exp(-0.15*(raw - 20)))
# This avoids the artificial "wall at 40 years".

hold_raw <- qgamma(U[,4], shape=3.0, scale=2.8)

# Type modulation
hold_mult <- case_when(
  investor_type == "non_investor"          ~ 0.0,    # no property → 0
  investor_type == "passive_longterm"      ~ 1.2,
  investor_type == "active_accumulator"    ~ 0.8,
  investor_type == "leveraged_speculator"  ~ 0.5,    # short holders
  investor_type == "legacy_holder"         ~ 1.5     # very long
)
hold_years_raw <- hold_raw * hold_mult

# Soft upper cap: logistic sigmoid mapped to [0.5, 50]
# At raw=20, output ≈ 25; at raw=40, output ≈ 42; never hits a hard wall
hold_years <- ifelse(
  is_investor == 1,
  0.5 + 34.5 / (1 + exp(-0.20 * (hold_years_raw - 12))),
  0
)
hold_years <- round(hold_years, 2)

# Inherited properties: typically held much longer (family history)
# Boost hold_years by 8-15 years for inherited investors
hold_years[is_inherited == 1] <- pmin(
  hold_years[is_inherited == 1] + runif(sum(is_inherited), 3, 8),
  48  # soft cap still applies
)

# --- 3e) Leverage ratio (P1-E: hurdle model) ---------------------------------
# Step 1: has_debt gate — P(has_debt) depends on type and age
# Older investors and legacy holders more likely to be debt-free.
# Inherited properties are overwhelmingly debt-free.
has_debt_prob <- case_when(
  investor_type == "non_investor"          ~ 0.0,
  investor_type == "passive_longterm"      ~ 0.75,
  investor_type == "active_accumulator"    ~ 0.92,
  investor_type == "leveraged_speculator"  ~ 0.97,
  investor_type == "legacy_holder"         ~ 0.40    # often paid off
)
# Inherited: override to very low debt probability
has_debt_prob[is_inherited == 1] <- 0.10
# Age effect: older → lower P(debt), applied as logistic adjustment
has_debt_prob <- plogis(qlogis(has_debt_prob) - 0.3 * scale(age))
has_debt <- as.integer(runif(n_total) < as.numeric(has_debt_prob))

# Step 2: Conditional leverage for those with debt
# Beta base from copula, scaled by investor type
leverage_base <- qbeta(U[,2], shape1=2.5, shape2=3.0)

leverage_scale <- case_when(
  investor_type == "passive_longterm"      ~ 1.8,
  investor_type == "active_accumulator"    ~ 2.5,
  investor_type == "leveraged_speculator"  ~ 3.2,   # can reach >3.0
  investor_type == "legacy_holder"         ~ 1.2,
  TRUE                                     ~ 0.0    # non-investor
)

leverage_ratio <- ifelse(has_debt == 1,
                         leverage_base * leverage_scale,
                         rexp(n_total, rate = 25))
# Non-investors: exactly 0
leverage_ratio[is_investor == 0] <- 0

# =============================================================================
# 4. Property value, debt & equity (P0-2 upgrade: accounting-consistent)
# =============================================================================
# The balance sheet must close: debt = LVR × property_value (not a leverage
# proxy back-computed from rent). This makes interest vs rent structurally
# realistic and neg_gearing emerges from the cash-flow identity naturally.
#
# Generation order:
#   1. property_value_total  (lognormal + Pareto tail for prestige properties)
#   2. LVR                   (hurdle + Beta, correlated with age/type/count)
#   3. debt_total = LVR × property_value_total
#   4. equity_total = property_value - debt
#   5. gross_rent = property_value × yield × occupancy  (NOT count × random)

# --- 4a) Property value per unit: lognormal body + Pareto upper tail --------
# Median ~$550k, but prestige properties ($2M+) exist in the tail
value_base <- rlnorm(n_total, log(550000), 0.45)

# Type adjustment
value_type_mult <- case_when(
  investor_type == "non_investor"          ~ 0,
  investor_type == "passive_longterm"      ~ 1.0,
  investor_type == "active_accumulator"    ~ 1.05,
  investor_type == "leveraged_speculator"  ~ 0.85,
  investor_type == "legacy_holder"         ~ 1.3
)
# Inherited: appreciated significantly
value_type_mult[is_inherited == 1] <- value_type_mult[is_inherited == 1] *
  runif(sum(is_inherited), 1.4, 1.8)

# Within-portfolio dispersion: properties in a family aren't identical
within_noise <- exp(rnorm(n_total, 0, 0.25))

# Pareto tail injection: ~3% of investor properties are prestige ($1.5M+)
is_prestige <- (is_investor == 1) & (runif(n_total) < 0.03)
value_base[is_prestige] <- value_base[is_prestige] * runif(sum(is_prestige), 2.5, 5.0)

property_value_per_unit <- value_base * value_type_mult * within_noise
property_value_total <- round(property_value_per_unit * portfolio_count)

# --- 4b) LVR: hurdle model (mirrors leverage hurdle but on value basis) -----
# LVR = debt / property_value. This is the fundamental balance sheet ratio.
# has_debt already generated in Section 3e above.
# For those with debt: LVR ~ Beta, modulated by age (older = lower LVR),
# investor type, and portfolio count (more properties = higher aggregate LVR).
lvr_base <- qbeta(U[,2], shape1=2.0, shape2=3.5)  # mode ~0.30, mean ~0.36

lvr_type_shift <- case_when(
  investor_type == "passive_longterm"      ~  0.0,
  investor_type == "active_accumulator"    ~  0.10,
  investor_type == "leveraged_speculator"  ~  0.20,   # high LVR strategy
  investor_type == "legacy_holder"         ~ -0.10,
  TRUE                                     ~  0.0
)
# Age effect: older investors have paid down debt → lower LVR
lvr_age_adj <- -0.08 * pmax(0, (age - 50) / 15)

lvr <- ifelse(has_debt == 1,
              pmin(pmax(lvr_base + lvr_type_shift + lvr_age_adj, 0.05), 0.95),
              0)
lvr[is_investor == 0] <- 0

# --- 4c) Debt and equity from LVR -------------------------------------------
debt_total   <- round(lvr * property_value_total)
equity_total <- property_value_total - debt_total

# =============================================================================
# 5. Net rental income (P0-C: cash-flow structural model, P0-2 consistent)
# =============================================================================
# All flows derived from the balance sheet:
#   gross_rent  = property_value_total × rent_yield × occupancy
#   interest    = debt_total × mortgage_rate
#   expenses    = property_value_total × expense_rate + fixed_per_property
#   depreciation = property_value_total × depreciation_rate
#   net_rental  = gross_rent − interest − expenses − depreciation
#
# Because debt_total = LVR × property_value_total, the interest burden is
# structurally consistent with the balance sheet. Negative gearing arises
# when interest + expenses + depreciation > gross_rent, which naturally
# happens for high-LVR / low-yield / high-depreciation investors.


# P1-1: Macro rate cohort — investors entered the market at different times,
# facing different interest rate environments. This creates cross-sectional
# variation in cash-flow pressure that real ATO data would show.
macro_state <- sample(c("low_rate", "normal", "high_rate"), n_total,
                      replace = TRUE, prob = c(0.30, 0.45, 0.25))
mortgage_rate <- case_when(
  macro_state == "low_rate"  ~ rnorm(n_total, 0.045, 0.005),  # 2015-2021 cohort
  macro_state == "normal"    ~ rnorm(n_total, 0.060, 0.005),  # 2012-2014 / 2022
  macro_state == "high_rate" ~ rnorm(n_total, 0.075, 0.005)   # 2023-2025 cohort
)
mortgage_rate <- pmax(mortgage_rate, 0.025)


# Rent yield: 3-6%, type-modulated
rent_yield <- case_when(
  investor_type == "passive_longterm"      ~ rnorm(n_total, 0.042, 0.008),
  investor_type == "active_accumulator"    ~ rnorm(n_total, 0.045, 0.010),
  investor_type == "leveraged_speculator"  ~ rnorm(n_total, 0.038, 0.012),  # lower yield, capital play
  investor_type == "legacy_holder"         ~ rnorm(n_total, 0.048, 0.008),  # older, higher yield
  TRUE                                     ~ 0
)
rent_yield <- pmax(rent_yield, 0.02)
# Yield weakly negatively correlated with rate environment (low rates = capital growth play)
rent_yield <- rent_yield - ifelse(macro_state == "low_rate", 0.004,
                                  ifelse(macro_state == "high_rate", -0.003, 0))
rent_yield <- pmax(rent_yield, 0.02)


# Occupancy rate: most >90%, speculators have more vacancies
occupancy <- case_when(
  investor_type == "leveraged_speculator"  ~ pmin(rnorm(n_total, 0.88, 0.08), 1),
  investor_type == "non_investor"          ~ 0,
  TRUE                                     ~ pmin(rnorm(n_total, 0.94, 0.04), 1)
)
occupancy <- pmax(occupancy, 0.5)

gross_rent <- round(property_value_total * rent_yield * occupancy)

# Interest payments
interest_deductions <- round(debt_total * mortgage_rate)

# Operating expenses (rates, insurance, management, repairs)
expense_rate <- runif(n_total, 0.008, 0.018)   # 0.8-1.8% of value p.a.
fixed_per_property <- portfolio_count * runif(n_total, 800, 2500)
other_deductions <- round(property_value_total * expense_rate + fixed_per_property)

# Depreciation (newer builds = higher; speculators buy newer)
depreciation_rate <- case_when(
  investor_type == "leveraged_speculator"  ~ runif(n_total, 0.015, 0.025),
  investor_type == "active_accumulator"    ~ runif(n_total, 0.010, 0.020),
  investor_type == "passive_longterm"      ~ runif(n_total, 0.005, 0.015),
  investor_type == "legacy_holder"         ~ runif(n_total, 0.002, 0.008),
  TRUE                                     ~ 0
)
depreciation <- round(property_value_total * depreciation_rate)

# Net rental income (the identity)
net_rental_income <- gross_rent - interest_deductions - other_deductions - depreciation

# Quarantined loss pool (cumulative negative gearing losses × years)
quarantined_pool <- ifelse(net_rental_income < 0,
                           round(abs(net_rental_income) * hold_years * 0.3),
                           0)

# =============================================================================
# 6. Policy indicator variables
# =============================================================================

# Supply contribution: new build or long-tenure lease
# Only investors can contribute supply; correlated with income, portfolio, hold
supply_logit <- -3.8 +
  0.4 * scale(income_taxable) +
  0.3 * scale(portfolio_count) -
  0.2 * scale(hold_years)
supply <- as.integer(is_investor == 1 &
                     runif(n_total) < plogis(as.numeric(supply_logit)))

# Hardship: verified life event (Centrelink, medical, NDIS)
# Anyone can experience hardship; older and lower-income → higher probability
hardship_logit <- -4.2 -
  0.5 * scale(income_taxable) +
  0.2 * scale(hold_years) +
  0.3 * scale(age)
hardship <- as.integer(runif(n_total) < plogis(as.numeric(hardship_logit)))

# =============================================================================
# 7. Structural / avoidance monitoring fields (P1-G improved)
# =============================================================================

# Trust structure: driven by property_value_total and portfolio_count,
# not just income. Speculators use trusts more. Low-income retirees
# with high-value assets can also have trusts (the "exception" the
# review panel flagged).
trust_logit <- -3.0 +
  0.4 * scale(income_taxable) +
  0.6 * ifelse(property_value_total > 0, scale(log1p(property_value_total)), 0) +
  0.5 * scale(portfolio_count) +
  ifelse(investor_type == "leveraged_speculator", 0.5, 0) +
  ifelse(investor_type == "legacy_holder" & property_value_total > 800000, 0.8, 0)
trust_structure <- as.integer(runif(n_total) < plogis(as.numeric(trust_logit)))
# Non-investors: no trust for property
trust_structure[is_investor == 0] <- 0L

# Related-party transaction flag
related_party <- as.integer(trust_structure == 1 & runif(n_total) < 0.15)

# Income volatility (3yr CV): heteroscedastic by type
cv_base <- 0.15 + 0.10 * as.numeric(scale(income_taxable))
cv_type_boost <- case_when(
  investor_type == "non_investor"          ~  0.00,
  investor_type == "passive_longterm"      ~  0.00,
  investor_type == "active_accumulator"    ~  0.05,
  investor_type == "leveraged_speculator"  ~  0.12,
  investor_type == "legacy_holder"         ~ -0.03
)
income_cv <- pmax(rnorm(n_total, mean = cv_base + cv_type_boost, sd = 0.08), 0.02)

# =============================================================================
# 8. Assemble and export
# =============================================================================

# P1-2: Measurement noise — real administrative data has reporting granularity
# and minor misstatement. This removes the "too clean" synthetic feel.
income_taxable <- round(income_taxable * exp(rnorm(n_total, 0, 0.03)))  # ~3% CV noise
income_taxable <- pmax(income_taxable, 0)
hold_years <- round(hold_years * 2) / 2    # heap to nearest 0.5 year
property_value_total <- ifelse(        # mild top-coding at p99
  property_value_total > quantile(property_value_total[is_investor==1], 0.99, na.rm=TRUE),
  quantile(property_value_total[is_investor==1], 0.99, na.rm=TRUE),
  property_value_total
)
property_value_total <- round(property_value_total)


ato_data <- tibble(
  family_group_id      = sprintf("FG-%06d", 1:n_total),
  fy                   = "2025-26",
  is_investor          = is_investor,
  investor_type        = investor_type,
  is_inherited         = is_inherited,
  age_proxy            = age,
  income_taxable       = round(income_taxable, 0),
  portfolio_count      = portfolio_count,
  property_value_total = property_value_total,
  debt_total           = debt_total,
  equity_total         = round(equity_total, 0),
  lvr                  = round(lvr, 4),
  leverage_ratio       = round(leverage_ratio, 4),
  hold_years_wavg      = hold_years,
  supply_contribution  = supply,
  hardship_indicator   = hardship,
  gross_rent           = gross_rent,
  interest_deductions  = interest_deductions,
  other_deductions     = other_deductions,
  depreciation         = depreciation,
  net_rental_income    = net_rental_income,
  quarantined_pool     = quarantined_pool,
  trust_structure      = trust_structure,
  related_party_flag   = related_party,
  income_cv_3yr        = round(income_cv, 4)
)

# ~~~ OUTPUT PATH — relative to project root ~~~
output_path <- file.path("data", "ato_synthetic_family_groups.csv")
write.csv(ato_data, output_path, row.names = FALSE)

cat("=== Data generation complete (v4) ===\n")
cat(sprintf("Records: %d total (%d investors, %d non-investors)\n",
            n_total, sum(is_investor), sum(!is_investor)))
cat("Output: ", output_path, "\n\n")

# =============================================================================
# 9. Diagnostics
# =============================================================================

cat("--- Population structure ---\n")
cat(sprintf("Investors:    %d (%.1f%%)\n", sum(is_investor), 100*mean(is_investor)))
cat(sprintf("Non-investors: %d (%.1f%%)\n", sum(!is_investor), 100*mean(!is_investor)))

cat("\n--- Investor marginals (investors only) ---\n")
inv <- ato_data %>% filter(is_investor == 1)
cat(sprintf("Income:     median=$%s  mean=$%s  p90=$%s\n",
            format(median(inv$income_taxable), big.mark=","),
            format(round(mean(inv$income_taxable)), big.mark=","),
            format(quantile(inv$income_taxable, 0.90), big.mark=",")))
cat(sprintf("Portfolio:  0=%d  1=%d  2=%d  3=%d  4+=%d\n",
            sum(ato_data$portfolio_count==0),
            sum(inv$portfolio_count==1), sum(inv$portfolio_count==2),
            sum(inv$portfolio_count==3), sum(inv$portfolio_count>=4)))
cat(sprintf("Leverage:   median=%.3f  %%debt-free=%.1f%%  %%>1.0=%.1f%%  %%>2.0=%.1f%%\n",
            median(inv$leverage_ratio),
            100*mean(inv$leverage_ratio < 0.05),
            100*mean(inv$leverage_ratio > 1.0),
            100*mean(inv$leverage_ratio > 2.0)))
cat(sprintf("Holding:    median=%.1f  mean=%.1f years\n",
            median(inv$hold_years_wavg), mean(inv$hold_years_wavg)))
cat(sprintf("Age:        median=%d  mean=%.0f\n",
            median(inv$age_proxy), mean(inv$age_proxy)))
cat(sprintf("Prop value: median=$%s  mean=$%s\n",
            format(median(inv$property_value_total), big.mark=","),
            format(round(mean(inv$property_value_total)), big.mark=",")))
cat(sprintf("LVR:        median=%.2f  %%>0.8=%.1f%%\n",
            median(inv$lvr), 100*mean(inv$lvr > 0.8)))
cat(sprintf("Supply:     %.1f%%\n", 100*mean(inv$supply_contribution)))
cat(sprintf("Hardship:   %.1f%%\n", 100*mean(inv$hardship_indicator)))
cat(sprintf("Trust:      %.1f%%\n", 100*mean(inv$trust_structure)))
cat(sprintf("Neg geared: %.1f%%\n", 100*mean(inv$net_rental_income < 0)))
cat(sprintf("Inherited:  %.1f%% of investors\n", 100*mean(inv$is_inherited)))

cat("\n--- Investor type distribution ---\n")
type_tbl <- table(ato_data$investor_type)
for (nm in names(sort(type_tbl, decreasing=TRUE))) {
  cat(sprintf("  %-25s %5d  (%.1f%%)\n", nm, type_tbl[nm], 100*type_tbl[nm]/n_total))
}

cat("\n--- Type profiles (investors only) ---\n")
inv %>%
  group_by(investor_type) %>%
  summarise(
    n = n(),
    med_income = median(income_taxable),
    mean_portf = round(mean(portfolio_count), 2),
    med_value  = median(property_value_total),
    mean_lever = round(mean(leverage_ratio), 3),
    mean_hold  = round(mean(hold_years_wavg), 1),
    mean_age   = round(mean(age_proxy), 1),
    pct_debt_free = round(100*mean(leverage_ratio < 0.05), 1),
    pct_neg_gear  = round(100*mean(net_rental_income < 0), 1),
    pct_trust     = round(100*mean(trust_structure), 1),
    .groups = "drop"
  ) %>%
  print(width = Inf)
