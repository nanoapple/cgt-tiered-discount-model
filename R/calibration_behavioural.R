# =============================================================================
# 02_calibration_model.R  (v7 — banded taper with smoothstep transitions)
# Appendix A: Internal analytical model for calibration and monitoring
# =============================================================================
# INPUT:  ato_synthetic_family_groups.csv  (produced by 01_generate_data.R)
# OUTPUT: Diagnostic plots + tier summary table
#
# Pipeline:
#   1. Load & filter to investors (portfolio_count ≥ 1)
#   2. Percentile standardisation → composite index F
#   3. BANDED TAPER CALIBRATION:
#      - Tier 0: rule-based flat d = 0.625 (compassionate carve-out)
#      - Tiers 1-3: discount BANDS with linear decrease + curvature p
#        T1: [30%, 25%], T2: [20%, 15%], T3: [5%, 0%]
#      - Smoothstep taper at T1|T2 and T2|T3 boundaries (eliminates cliffs)
#   4. Joint optimisation of (p, c1, c2, c3) with band-compliance loss
#   5. Summary statistics, band compliance, calibration quality
#   6. Diagnostic visualisations with band region overlays
#
# v7 STRUCTURAL CHANGE: Replaces logistic curve with banded taper function.
# The logistic created an uncontrollable single-curve mapping where each tier
# got "whatever the curve happened to give". The banded taper gives each tier
# an independently specifiable discount range, with smooth transitions between
# tiers. This is politically defensible ("discount bands") and eliminates the
# cliff effects at T1|T2 and T2|T3 boundaries.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# =============================================================================
# 0. Load data & filter to investors
# =============================================================================
input_path <- file.path("data", "ato_synthetic_family_groups.csv")

raw_data <- read.csv(input_path) %>% as_tibble()
cat(sprintf("Loaded %d family groups from %s\n", nrow(raw_data), input_path))

ato_data <- raw_data %>% filter(portfolio_count >= 1)
cat(sprintf("Filtered to %d investors (excluded %d non-investors)\n\n",
            nrow(ato_data), nrow(raw_data) - nrow(ato_data)))


# =============================================================================
# 1. Percentile standardisation (truncated to [0.01, 0.99])
# =============================================================================
# Each continuous variable is standardised to its truncated percentile rank
# WITHIN the investor population, putting them on a common [0,1] scale.
# Binary policy flags (S,H) are kept as {0,1} indicators.

pct_trunc <- function(x, lo=0.01, hi=0.99) {
  pmin(pmax(rank(x, ties.method="average") / length(x), lo), hi)
}

ato_data <- ato_data %>%
  mutate(
    I_hat = pct_trunc(income_taxable),
    L_hat = pct_trunc(leverage_ratio),
    A_hat = pct_trunc(portfolio_count),
    V_hat = pct_trunc(property_value_total),
    t_hat = pct_trunc(hold_years_wavg),
    S_hat = ifelse(supply_contribution == 1, 1.0, 0.0),
    H_hat = ifelse(hardship_indicator  == 1, 1.0, 0.0)
  )

# =============================================================================
# 2. Composite investor index F (weights: proposal / data-driven / blended)
# =============================================================================
# F_raw = + w_I*I_hat + w_L*L_hat + w_A*A_hat + w_V*V_hat
#         - w_t*t_hat - w_S*S_hat - w_H*H_hat
#
# Interpretation:
#  - Higher F implies stronger "tightening" (lower discount)
#  - Holding period (t), supply contribution (S) and hardship (H) reduce F
#    (protective factors)

# --- Choose which weights to use ---
weight_mode <- "proposal"   # "proposal" | "data" | "blend"
lambda <- 0.30              # only used when weight_mode == "blend"

# Proposal (normative) weights
w_prop <- c(I=0.30, L=0.20, A=0.15, V=0.15, t=0.12, S=0.05, H=0.03)

# Data-driven magnitudes (from 03_weight_justification.R)
# NOTE: these are magnitude-only weights; we keep policy sign convention above.
w_data <- c(I=0.007, L=0.445, A=0.132, V=0.033, t=0.355, S=0.014, H=0.014)

# Resolve final weights
w <- switch(
  weight_mode,
  proposal = w_prop,
  data     = w_data,
  blend    = (1 - lambda) * w_prop + lambda * w_data,
  stop("Invalid weight_mode. Use 'proposal', 'data', or 'blend'.")
)

cat("=== Composite index weights ===\n")
cat(sprintf("Mode: %s%s\n",
            weight_mode,
            if (weight_mode == "blend") sprintf(" (lambda=%.2f)", lambda) else ""))
cat(paste(sprintf("  %s = %.3f", names(w), w), collapse = "\n"), "\n\n")

# --- Build F_raw and normalise to [0,1] ---
# (Min-max normalisation makes F comparable even when weight sets change.)
norm_minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

ato_data <- ato_data %>%
  mutate(
    F_raw =  w["I"]*I_hat + w["L"]*L_hat + w["A"]*A_hat + w["V"]*V_hat
    - w["t"]*t_hat - w["S"]*S_hat - w["H"]*H_hat,
    F = norm_minmax(F_raw)
  )


# =============================================================================
# 3–4. BANDED TAPER CALIBRATION (v7)
# =============================================================================
#
# ARCHITECTURAL CHANGE from v6:
#
# v6 used a logistic curve for T1-T3 with a flat Tier 0 rule. The logistic
# creates a single smooth curve, but this has two political/policy problems:
#
#   1. The T0|T1 boundary shows a massive cliff (~62.5% → ~30%), creating
#      a horizontal equity objection and a gaming incentive at that boundary.
#   2. A single-parameter curve (k) cannot independently control the discount
#      level within each tier — Tiers are "whatever the curve happens to give".
#
# SOLUTION: Replace the logistic with a BANDED TAPER function.
#
# Policy architecture:
#   - Each tier defines a DISCOUNT BAND (not a point target):
#       T0: flat 62.5% (rule-based carve-out, unchanged)
#       T1: 30% → 25% (decreasing within tier as F increases)
#       T2: 20% → 15%
#       T3:  5% →  0%
#   - Within each tier, discount decreases linearly (with optional curvature p)
#   - At tier boundaries (T1|T2, T2|T3), a smoothstep taper blends the
#     adjacent band functions over a narrow window (±δ), eliminating cliffs
#   - T0|T1 boundary retains its step (policy carve-out), but the T1 band
#     starts at 30% not 25%, reducing the cliff from ~33pp to ~33pp
#     (T0 is deliberately distinct — like a tax-free threshold)
#
# Benefits:
#   - No cliff at T1|T2 or T2|T3 (smoothstep ensures C¹ continuity)
#   - Each tier's discount range is independently specifiable
#   - "Band compliance" replaces "mean matching" — more realistic loss function
#   - Fewer optimisation parameters (p, c1, c2, c3) — no k, F0
#   - Politically defensible: "discount bands" are intuitive to stakeholders
# =============================================================================

# --- Policy parameters: discount bands ---
d_T0 <- 0.625  # Tier 0: flat rule (compassionate carve-out)

band <- list(
  T1 = c(lo=0.25, hi=0.30),   # Long-term / passive: 30% down to 25%
  T2 = c(lo=0.15, hi=0.20),   # Moderate: 20% down to 15%
  T3 = c(lo=0.00, hi=0.05)    # No discount: 5% down to 0%
)

# Band midpoints (for summary/monitoring MSE)
band_mid <- c(T1=0.275, T2=0.175, T3=0.025)

target_shares <- c(0.05, 0.55, 0.25, 0.15)
target_pop_mean <- target_shares[1]*d_T0 +
                   target_shares[2]*band_mid["T1"] +
                   target_shares[3]*band_mid["T2"] +
                   target_shares[4]*band_mid["T3"]

delta <- 0.015  # Taper half-width in F-space (smoothstep transition zone)

F_vec <- ato_data$F
n_obs <- length(F_vec)

# --- Quantile anchors ---
c1_anchor <- as.numeric(quantile(F_vec, probs = target_shares[1]))
q_anchor_23 <- as.numeric(quantile(F_vec, probs = cumsum(target_shares[1:3])[2:3]))
cat(sprintf("Quantile anchors: c1(Q5%%)=%.4f  c2(Q60%%)=%.4f  c3(Q85%%)=%.4f\n",
            c1_anchor, q_anchor_23[1], q_anchor_23[2]))

# --- Hyperparameters ---
rho     <- c(10,3,3,8)   # (E) share matching weights (T0/T3 endpoints higher)
lam_bnd <- c(8,12,10)     # (B) band compliance weights (T3 highest: political sensitivity)
lam_mid <- 1.0           # (B2) band midpoint MSE weight (light: monitoring, not primary)
lam0    <- 5             # (A) overall mean weight
gamma   <- 2             # (C) boundary inequity (T1|T2, T2|T3)
tau     <- 1.5           # (F) boundary mass penalty
beta    <- 0.3           # (D) tier-size floor
nu_c1   <- 2.0           # (G) c1 drift penalty
nu_23   <- 0.5           # (G) c2,c3 drift penalty
alpha_p <- 0.01          # (D) p regularisation toward 1.0

# Dynamic n_min per tier
n_min_vec <- pmax(30, floor(0.5 * target_shares * n_obs))

cat("\n=== Banded taper calibration ===\n")
cat(sprintf("  T0: flat d=%.3f\n", d_T0))
cat(sprintf("  T1: band [%.2f, %.2f], mid=%.3f\n", band$T1["lo"], band$T1["hi"], band_mid["T1"]))
cat(sprintf("  T2: band [%.2f, %.2f], mid=%.3f\n", band$T2["lo"], band$T2["hi"], band_mid["T2"]))
cat(sprintf("  T3: band [%.2f, %.2f], mid=%.3f\n", band$T3["lo"], band$T3["hi"], band_mid["T3"]))
cat(sprintf("  Taper half-width: delta=%.3f\n", delta))
cat(sprintf("  Target pop mean: %.4f\n", target_pop_mean))
cat(sprintf("  Target shares: %.0f%% / %.0f%% / %.0f%% / %.0f%%\n",
            100*target_shares[1], 100*target_shares[2],
            100*target_shares[3], 100*target_shares[4]))

# =============================================================================
# Core functions: banded taper
# =============================================================================

clamp01 <- function(x) pmin(pmax(x, 0), 1)

smoothstep <- function(z) {
  z <- clamp01(z)
  z * z * (3 - 2 * z)
}

tier_linear <- function(F, a, b, hi, lo, p = 1.0) {
  u <- clamp01((F - a) / (b - a))
  u <- u^p
  hi + (lo - hi) * u
}

d_banded_taper <- function(F, c1, c2, c3, band, delta = 0.02, p = 1.0) {
  # Core tier functions (each decreases from hi to lo across its F range)
  d1 <- tier_linear(F, c1, c2, band$T1["hi"], band$T1["lo"], p)
  d2 <- tier_linear(F, c2, c3, band$T2["hi"], band$T2["lo"], p)
  d3 <- tier_linear(F, c3, 1.0, band$T3["hi"], band$T3["lo"], p)

  # Start with hard tier assignment
  d <- ifelse(F < c2, d1, ifelse(F < c3, d2, d3))

  # Smoothstep taper around c2 (T1|T2 boundary)
  z2 <- (F - (c2 - delta)) / (2 * delta)
  w2 <- smoothstep(z2)
  in2 <- (F >= (c2 - delta)) & (F <= (c2 + delta))
  d[in2] <- (1 - w2[in2]) * d1[in2] + w2[in2] * d2[in2]

  # Smoothstep taper around c3 (T2|T3 boundary)
  z3 <- (F - (c3 - delta)) / (2 * delta)
  w3 <- smoothstep(z3)
  in3 <- (F >= (c3 - delta)) & (F <= (c3 + delta))
  d[in3] <- (1 - w3[in3]) * d2[in3] + w3[in3] * d3[in3]

  d
}

band_penalty <- function(d, lo, hi) {
  # Hinge-squared: zero penalty inside band, quadratic outside
  mean(pmax(0, d - hi)^2 + pmax(0, lo - d)^2)
}

# =============================================================================
# Joint loss function: 4 parameters (p, c1, c2, c3)
# =============================================================================

joint_loss <- function(par) {
  p  <- par[1]
  c1 <- par[2]; c2 <- par[3]; c3 <- par[4]

  # Hard constraints
  if (p < 0.5 || p > 4.0) return(1e6)
  if (c1 >= c2 || c2 >= c3) return(1e6)
  if ((c2 - c1) < 0.03 || (c3 - c2) < 0.03) return(1e6)
  if (c1 < 0.005 || c3 > 0.99) return(1e6)

  # --- Compute d(F): banded taper for T1-T3, flat for T0 ---
  d_cont <- d_banded_taper(F_vec, c1, c2, c3, band = band, delta = delta, p = p)
  d <- ifelse(F_vec < c1, d_T0, d_cont)

  # --- Assign tiers ---
  tier <- ifelse(F_vec < c1, 0L,
          ifelse(F_vec < c2, 1L,
          ifelse(F_vec < c3, 2L, 3L)))

  # --- Tier counts ---
  n_tier <- integer(4)
  for (j in 0:3) n_tier[j+1] <- sum(tier == j)

  # (A) Overall population mean
  d_bar <- mean(d)
  loss_A <- lam0 * (d_bar - target_pop_mean)^2

  # (B) Band compliance — PRIMARY loss for T1-T3
  # Hinge-squared: zero if d falls within the band, quadratic penalty if outside
  idx1 <- which(tier == 1L)
  idx2 <- which(tier == 2L)
  idx3 <- which(tier == 3L)
  loss_band <- 0
  if (length(idx1) > 0) loss_band <- loss_band + lam_bnd[1] * band_penalty(d[idx1], band$T1["lo"], band$T1["hi"])
  if (length(idx2) > 0) loss_band <- loss_band + lam_bnd[2] * band_penalty(d[idx2], band$T2["lo"], band$T2["hi"])
  if (length(idx3) > 0) loss_band <- loss_band + lam_bnd[3] * band_penalty(d[idx3], band$T3["lo"], band$T3["hi"])

  # (B2) Band midpoint MSE — light monitoring constraint
  d_policy_mid <- ifelse(F_vec < c1, d_T0,
                  ifelse(F_vec < c2, band_mid["T1"],
                  ifelse(F_vec < c3, band_mid["T2"], band_mid["T3"])))
  idx_13 <- which(tier > 0L)
  loss_mid <- if (length(idx_13) > 0) lam_mid * mean((d_cont[idx_13] - d_policy_mid[idx_13])^2) else 0

  # (C) Boundary inequity Δ² (T1|T2 and T2|T3 only; T0|T1 is rule boundary)
  eps <- 0.02
  m_min <- 30
  bgap_detail <- function(cb, lt, rt) {
    L <- which(F_vec >= cb - eps & F_vec < cb & tier == lt)
    R <- which(F_vec >= cb & F_vec < cb + eps & tier == rt)
    nL <- length(L); nR <- length(R)
    gap <- if (nL >= 5 && nR >= 5) abs(mean(d[L]) - mean(d[R])) else 0
    list(gap = gap, nL = nL, nR = nR)
  }
  b2 <- bgap_detail(c2, 1, 2)
  b3 <- bgap_detail(c3, 2, 3)
  loss_C <- gamma * (b2$gap^2 + b3$gap^2)

  # (D) Regularisation: p toward 1.0 + tier-size floors
  pen_small <- beta * sum(pmax(0, n_min_vec - n_tier)^2)
  loss_D <- alpha_p * (p - 1.0)^2 + pen_small

  # (E) Tier share matching
  shares <- n_tier / n_obs
  loss_E <- sum(rho * (shares - target_shares)^2)

  # (F) Boundary mass penalty
  pi2 <- max(0, m_min - b2$nL)^2 + max(0, m_min - b2$nR)^2
  pi3 <- max(0, m_min - b3$nL)^2 + max(0, m_min - b3$nR)^2
  loss_F <- tau * (pi2 + pi3)

  # (G) Cutpoint drift penalty
  drift_c1 <- (c1 - c1_anchor)^2
  drift_23 <- (c2 - q_anchor_23[1])^2 + (c3 - q_anchor_23[2])^2
  loss_G <- nu_c1 * drift_c1 + nu_23 * drift_23

  return(loss_A + loss_band + loss_mid + loss_C + loss_D + loss_E + loss_F + loss_G)
}

# --- Multi-start optimisation: 4 parameters (p, c1, c2, c3) ---
qa <- c(c1_anchor, q_anchor_23)
starts_4d <- list(
  c(1.0, qa[1], qa[2], qa[3]),
  c(1.2, qa[1], qa[2], qa[3]),
  c(1.4, qa[1], qa[2], qa[3]),
  c(1.6, qa[1], qa[2], qa[3]),
  c(2.0, qa[1], qa[2], qa[3]),
  c(1.0, qa[1]-0.01, qa[2]-0.02, qa[3]-0.02),
  c(1.4, qa[1]+0.01, qa[2]+0.02, qa[3]+0.02),
  c(1.2, qa[1], qa[2]-0.03, qa[3]-0.01),
  c(1.6, qa[1]+0.01, qa[2]+0.03, qa[3]+0.01),
  c(1.8, qa[1]-0.01, qa[2]-0.04, qa[3]+0.02)
)

best_result <- list(value = Inf)
for (s in starts_4d) {
  res <- optim(par = s, fn = joint_loss, method = "Nelder-Mead",
               control = list(maxit = 50000, reltol = 1e-10))
  if (res$value < best_result$value) best_result <- res
}

p_param <- best_result$par[1]
cuts    <- best_result$par[2:4]

cat(sprintf("\nOptimised: p=%.3f\n", p_param))
cat(sprintf("Cutpoints: c1=%.4f  c2=%.4f  c3=%.4f\n", cuts[1], cuts[2], cuts[3]))
cat(sprintf("Joint loss: %.6f\n\n", best_result$value))

# --- Apply banded taper d(F) to data ---
ato_data <- ato_data %>%
  mutate(
    d_continuous = d_banded_taper(F, cuts[1], cuts[2], cuts[3],
                                  band = band, delta = delta, p = p_param),
    d_implied    = ifelse(F < cuts[1], d_T0, d_continuous)
  )

# Assign tiers using optimised cutpoints
ato_data <- ato_data %>%
  mutate(
    tier = case_when(
      F < cuts[1] ~ "Tier 0 (Compassionate)",
      F < cuts[2] ~ "Tier 1 (Long-term / passive)",
      F < cuts[3] ~ "Tier 2 (Moderate)",
      TRUE        ~ "Tier 3 (No discount)"
    ),
    tier_discount = case_when(
      F < cuts[1] ~ d_T0,
      F < cuts[2] ~ band_mid["T1"],
      F < cuts[3] ~ band_mid["T2"],
      TRUE        ~ band_mid["T3"]
    ),
    band_lo = case_when(
      F < cuts[1] ~ d_T0,
      F < cuts[2] ~ band$T1["lo"],
      F < cuts[3] ~ band$T2["lo"],
      TRUE        ~ band$T3["lo"]
    ),
    band_hi = case_when(
      F < cuts[1] ~ d_T0,
      F < cuts[2] ~ band$T1["hi"],
      F < cuts[3] ~ band$T2["hi"],
      TRUE        ~ band$T3["hi"]
    ),
    in_band = (d_implied >= band_lo) & (d_implied <= band_hi)
  )

# =============================================================================
# 5. Summary statistics
# =============================================================================

cat("=== Tier summary ===\n")
tier_summary <- ato_data %>%
  group_by(tier) %>%
  summarise(
    n              = n(),
    share          = round(n()/nrow(ato_data), 4),
    mean_d         = round(mean(d_implied), 4),
    band_range     = paste0("[", first(band_lo), ", ", first(band_hi), "]"),
    pct_in_band    = round(100*mean(in_band), 1),
    mean_income    = round(mean(income_taxable)),
    mean_portfolio = round(mean(portfolio_count), 2),
    mean_value     = round(mean(property_value_total)/1000),
    mean_leverage  = round(mean(leverage_ratio), 3),
    mean_hold_yrs  = round(mean(hold_years_wavg), 1),
    pct_neg_gear   = round(100*mean(net_rental_income < 0), 1),
    pct_trust      = round(100*mean(trust_structure), 1),
    .groups = "drop"
  )
print(tier_summary, width = Inf)

# Calibration quality
cat("\n=== Calibration quality ===\n")
cat(sprintf("Population mean d(F): %.4f  (target: %.4f, gap: %+.4f)\n",
            mean(ato_data$d_implied), target_pop_mean,
            mean(ato_data$d_implied) - target_pop_mean))
cat("  Tier 0 (Compassionate): d=0.6250 (EXACT by rule)\n")
for (t_name in c("Tier 1 (Long-term / passive)",
                  "Tier 2 (Moderate)", "Tier 3 (No discount)")) {
  idx <- ato_data$tier == t_name
  if (sum(idx) == 0) next
  md  <- mean(ato_data$d_implied[idx])
  blo <- ato_data$band_lo[idx][1]; bhi <- ato_data$band_hi[idx][1]
  pib <- round(100*mean(ato_data$in_band[idx]), 1)
  cat(sprintf("  %s: mean_d=%.4f  band=[%.2f,%.2f]  in-band=%.1f%%\n",
              t_name, md, blo, bhi, pib))
}

# Band compliance summary
cat("\n=== Band compliance (T1-T3) ===\n")
idx_13 <- ato_data$tier != "Tier 0 (Compassionate)"
pct_in_band_all <- round(100*mean(ato_data$in_band[idx_13]), 1)
mse_mid <- mean((ato_data$d_implied[idx_13] - ato_data$tier_discount[idx_13])^2)
cat(sprintf("Overall in-band rate (T1-T3): %.1f%%\n", pct_in_band_all))
cat(sprintf("MSE vs band midpoint: %.6f\n", mse_mid))

# Loss component decomposition
cat("\n=== Loss decomposition ===\n")
d_all <- ato_data$d_implied
tier_int <- ifelse(ato_data$F < cuts[1], 0, ifelse(ato_data$F < cuts[2], 1,
            ifelse(ato_data$F < cuts[3], 2, 3)))
n_tiers_final <- tapply(d_all, tier_int, length)
shares_final  <- n_tiers_final / nrow(ato_data)
cat(sprintf("  (A) Overall mean:     loss=%.6f\n", lam0*(mean(d_all)-target_pop_mean)^2))
# Band compliance per tier
for (j in 1:3) {
  idx_j <- which(tier_int == j)
  blo_j <- band[[j]]["lo"]; bhi_j <- band[[j]]["hi"]
  bp_j <- if (length(idx_j) > 0) band_penalty(d_all[idx_j], blo_j, bhi_j) else 0
  cat(sprintf("  (B) T%d band penalty:  %.6f  (wt=%g)\n", j, bp_j, lam_bnd[j]))
}
cat(sprintf("  (E) Tier shares:      loss=%.6f  (actual: %.1f%% / %.1f%% / %.1f%% / %.1f%%)\n",
            sum(rho*(shares_final-target_shares)^2),
            100*shares_final[1], 100*shares_final[2],
            100*shares_final[3], 100*shares_final[4]))
cat(sprintf("  (D) Curvature reg:    p=%.3f  penalty=%.6f\n", p_param, alpha_p*(p_param-1)^2))

cat("\n=== Correlation matrix (raw variables) ===\n")
cor_vars <- ato_data[, c("income_taxable", "leverage_ratio",
                          "portfolio_count", "property_value_total",
                          "hold_years_wavg")]
cor(cor_vars) %>% round(3) %>% print()

# =============================================================================
# 6. Diagnostic plots
# =============================================================================

tier_cols <- c(
  "Tier 0 (Compassionate)"       = "#27ae60",
  "Tier 1 (Long-term / passive)" = "#2980b9",
  "Tier 2 (Moderate)"            = "#f39c12",
  "Tier 3 (No discount)"         = "#c0392b"
)

# --- Plot 1: d(F) vs F with BAND REGIONS ---
band_rects <- tibble(
  xmin = c(cuts[1], cuts[2], cuts[3]),
  xmax = c(cuts[2], cuts[3], max(ato_data$F, na.rm=TRUE)),
  ymin = c(band$T1["lo"], band$T2["lo"], band$T3["lo"]),
  ymax = c(band$T1["hi"], band$T2["hi"], band$T3["hi"]),
  label = c("T1 band", "T2 band", "T3 band")
)

p1 <- ggplot(ato_data %>% sample_n(min(8000, nrow(ato_data))),
             aes(x=F, y=d_implied)) +
  # Band regions (light shading)
  geom_rect(data=band_rects, inherit.aes=FALSE,
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#3498db", alpha=0.10) +
  # Tier 0 region
  annotate("rect", xmin=0, xmax=cuts[1], ymin=0.61, ymax=0.64,
           fill="#27ae60", alpha=0.15) +
  geom_point(alpha=0.12, size=0.8, colour="#2c3e50") +
  geom_hline(yintercept=c(d_T0, band$T1["hi"], band$T1["lo"],
                           band$T2["hi"], band$T2["lo"],
                           band$T3["hi"], band$T3["lo"]),
             linetype="dotted", colour="grey60", alpha=0.4) +
  geom_vline(xintercept=cuts, linetype="dashed", colour="firebrick", alpha=0.6) +
  annotate("text", x=cuts, y=0.66,
           label=c("T0|T1","T1|T2","T2|T3"),
           hjust=-0.1, size=3, colour="firebrick") +
  annotate("text", x=0.01, y=d_T0+0.015, label="T0=62.5%",
           hjust=0, size=2.5, colour="#27ae60") +
  annotate("text", x=band_rects$xmin+0.01,
           y=(band_rects$ymin+band_rects$ymax)/2,
           label=band_rects$label, hjust=0, size=2.5, colour="#2980b9") +
  labs(title="Implied discount d(F) vs composite index F (banded taper)",
       subtitle=sprintf("Curvature p=%.2f | Taper δ=%.3f | Bands: T1[%.0f-%.0f%%] T2[%.0f-%.0f%%] T3[%.0f-%.0f%%]",
                         p_param, delta,
                         100*band$T1["lo"], 100*band$T1["hi"],
                         100*band$T2["lo"], 100*band$T2["hi"],
                         100*band$T3["lo"], 100*band$T3["hi"]),
       x="Composite investor index F", y="Implied discount rate d(F)") +
  theme_minimal()

# --- Plot 2: F distribution by tier ---
p2 <- ggplot(ato_data, aes(x=F, fill=tier)) +
  geom_histogram(bins=100, alpha=0.85) +
  geom_vline(xintercept=cuts, linetype="dashed", linewidth=0.5) +
  scale_fill_manual(values=tier_cols) +
  labs(title="Distribution of composite index F with tier boundaries",
       x="Composite investor index F", y="Count", fill="Tier") +
  theme_minimal()

# --- Plot 3: Boundary inequity (T1|T2 and T2|T3 only) ---
ineq_band <- 0.02
ineq <- tibble(
  cut = cuts[2:3],
  boundary = factor(c("T1 | T2", "T2 | T3"),
                    levels=c("T1 | T2", "T2 | T3"))
) %>%
  rowwise() %>%
  mutate(
    n_below = sum(ato_data$F >= cut-ineq_band & ato_data$F < cut),
    n_above = sum(ato_data$F >= cut & ato_data$F <= cut+ineq_band),
    lower_mean = ifelse(n_below > 0,
                        mean(ato_data$d_implied[ato_data$F >= cut-ineq_band & ato_data$F < cut]),
                        NA_real_),
    upper_mean = ifelse(n_above > 0,
                        mean(ato_data$d_implied[ato_data$F >= cut & ato_data$F <= cut+ineq_band]),
                        NA_real_),
    gap = ifelse(!is.na(lower_mean) & !is.na(upper_mean),
                 abs(lower_mean - upper_mean), 0)
  ) %>%
  ungroup()

p3 <- ggplot(ineq, aes(x=boundary, y=gap)) +
  geom_col(fill="#8e44ad", width=0.5) +
  geom_text(aes(label=paste0(round(gap*100,1),"pp")), vjust=-0.5, size=3.5) +
  labs(title="Horizontal inequity at tier boundaries (with smoothstep taper)",
       subtitle=sprintf("Taper δ=%.3f | Only T1|T2 and T2|T3 (T0|T1 is rule boundary)", delta),
       x="Tier boundary", y="Discount gap (percentage points)") +
  theme_minimal()

# --- Plot 4: Income vs portfolio by tier ---
p4 <- ggplot(ato_data %>% sample_n(min(10000, nrow(ato_data))),
             aes(x=income_taxable/1000, y=portfolio_count, colour=tier)) +
  geom_jitter(alpha=0.5, size=1.5, height=0.2) +
  scale_colour_manual(values=tier_cols) +
  scale_x_log10(labels=comma_format(suffix="k")) +
  labs(title="Family taxable income vs portfolio size by tier",
       x="Family taxable income ($k, log scale)", y="Investment properties",
       colour="Tier") +
  theme_minimal()

# --- Plot 5: Holding period vs leverage by tier ---
p5 <- ggplot(ato_data %>% sample_n(min(10000, nrow(ato_data))),
             aes(x=hold_years_wavg, y=leverage_ratio, colour=tier)) +
  geom_point(alpha=0.5, size=1.5) +
  scale_colour_manual(values=tier_cols) +
  labs(title="Holding period vs leverage ratio by tier",
       x="Weighted avg holding period (years)",
       y="Leverage ratio (interest/rent)",
       colour="Tier") +
  theme_minimal()

# --- Plot 6: Tier profile comparison ---
tier_profile <- ato_data %>%
  group_by(tier) %>%
  summarise(
    income_med  = median(income_taxable)/1000,
    portfolio   = mean(portfolio_count),
    value_med   = median(property_value_total)/100000,
    leverage    = mean(leverage_ratio),
    hold        = mean(hold_years_wavg),
    .groups = "drop"
  ) %>%
  pivot_longer(-tier, names_to="metric", values_to="value")

p6 <- ggplot(tier_profile, aes(x=metric, y=value, fill=tier)) +
  geom_col(position="dodge") +
  scale_fill_manual(values=tier_cols) +
  facet_wrap(~metric, scales="free", nrow=1) +
  labs(title="Tier profiles: how investor characteristics differ across tiers",
       x=NULL, y=NULL, fill="Tier") +
  theme_minimal() +
  theme(strip.text = element_text(face="bold"))

print(p1); print(p2); print(p3); print(p4); print(p5); print(p6)

cat("\n=== Calibration model complete ===\n")
