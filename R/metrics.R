# =============================================================================
# metrics.R — Tier summary, calibration quality, and loss decomposition
# =============================================================================
# Shared reporting functions used by both baseline and behavioural pipelines.
# =============================================================================

#' Assign tiers and band metadata to calibrated data
#'
#' @param ato_data  tibble with columns F, d_continuous, d_implied
#' @param cuts      numeric(3) optimised cutpoints c1, c2, c3
#' @param d_T0      Tier 0 flat discount (default 0.625)
#' @param band      list with T1, T2, T3 each containing lo/hi
#' @param band_mid  named numeric(3) band midpoints
#' @return tibble with tier, tier_discount, band_lo, band_hi, in_band columns
assign_tiers <- function(ato_data, cuts, d_T0, band, band_mid) {
  ato_data %>%
    dplyr::mutate(
      tier = dplyr::case_when(
        F < cuts[1] ~ "Tier 0 (Compassionate)",
        F < cuts[2] ~ "Tier 1 (Long-term / passive)",
        F < cuts[3] ~ "Tier 2 (Moderate)",
        TRUE        ~ "Tier 3 (No discount)"
      ),
      tier_discount = dplyr::case_when(
        F < cuts[1] ~ d_T0,
        F < cuts[2] ~ band_mid["T1"],
        F < cuts[3] ~ band_mid["T2"],
        TRUE        ~ band_mid["T3"]
      ),
      band_lo = dplyr::case_when(
        F < cuts[1] ~ d_T0,
        F < cuts[2] ~ band$T1["lo"],
        F < cuts[3] ~ band$T2["lo"],
        TRUE        ~ band$T3["lo"]
      ),
      band_hi = dplyr::case_when(
        F < cuts[1] ~ d_T0,
        F < cuts[2] ~ band$T1["hi"],
        F < cuts[3] ~ band$T2["hi"],
        TRUE        ~ band$T3["hi"]
      ),
      in_band = (d_implied >= band_lo) & (d_implied <= band_hi)
    )
}

#' Print tier summary table
print_tier_summary <- function(ato_data) {
  cat("=== Tier summary ===\n")
  tier_summary <- ato_data %>%
    dplyr::group_by(tier) %>%
    dplyr::summarise(
      n              = dplyr::n(),
      share          = round(dplyr::n()/nrow(ato_data), 4),
      mean_d         = round(mean(d_implied), 4),
      band_range     = paste0("[", dplyr::first(band_lo), ", ", dplyr::first(band_hi), "]"),
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
  invisible(tier_summary)
}

#' Print calibration quality diagnostics
print_calibration_quality <- function(ato_data, target_pop_mean, band, d_T0) {
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

  cat("\n=== Band compliance (T1-T3) ===\n")
  idx_13 <- ato_data$tier != "Tier 0 (Compassionate)"
  pct_in_band_all <- round(100*mean(ato_data$in_band[idx_13]), 1)
  mse_mid <- mean((ato_data$d_implied[idx_13] - ato_data$tier_discount[idx_13])^2)
  cat(sprintf("Overall in-band rate (T1-T3): %.1f%%\n", pct_in_band_all))
  cat(sprintf("MSE vs band midpoint: %.6f\n", mse_mid))
}

#' Print loss function decomposition
print_loss_decomposition <- function(ato_data, cuts, band, lam_bnd, lam0,
                                      target_pop_mean, rho, target_shares,
                                      alpha_p, p_param) {
  cat("\n=== Loss decomposition ===\n")
  d_all <- ato_data$d_implied
  tier_int <- ifelse(ato_data$F < cuts[1], 0, ifelse(ato_data$F < cuts[2], 1,
              ifelse(ato_data$F < cuts[3], 2, 3)))
  n_tiers_final <- tapply(d_all, tier_int, length)
  shares_final  <- n_tiers_final / nrow(ato_data)
  cat(sprintf("  (A) Overall mean:     loss=%.6f\n", lam0*(mean(d_all)-target_pop_mean)^2))
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
}
