# =============================================================================
# taper_functions.R — Reusable banded taper functions
# =============================================================================
# These functions implement the core banded taper mapping from composite
# investor index F to implied discount d(F). They are shared by both
# the baseline and behavioural calibration pipelines.
#
# Functions:
#   clamp01()         — clamp values to [0,1]
#   smoothstep()      — Hermite smoothstep for C¹ boundary blending
#   tier_linear()     — linear (or power-curved) descent within a tier
#   d_banded_taper()  — full banded taper mapping for Tiers 1-3
#   band_penalty()    — hinge-squared out-of-band penalty for loss function
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
