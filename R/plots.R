# =============================================================================
# plots.R — Diagnostic visualisations for calibration output
# =============================================================================

tier_cols <- c(
  "Tier 0 (Compassionate)"       = "#27ae60",
  "Tier 1 (Long-term / passive)" = "#2980b9",
  "Tier 2 (Moderate)"            = "#f39c12",
  "Tier 3 (No discount)"         = "#c0392b"
)

#' Generate all diagnostic plots
#' @param ato_data  calibrated tibble with tier, d_implied, F, etc.
#' @param cuts      numeric(3) optimised cutpoints
#' @param band      list with T1, T2, T3 band definitions
#' @param band_mid  named numeric(3) band midpoints
#' @param d_T0      Tier 0 flat discount
#' @param p_param   curvature parameter
#' @param delta     taper half-width
#' @return list of ggplot objects (p1..p6)
generate_plots <- function(ato_data, cuts, band, band_mid, d_T0, p_param, delta) {

  # --- Plot 1: d(F) vs F with BAND REGIONS + ENVELOPE CURVE ---
  band_rects <- tibble::tibble(
    xmin = c(cuts[1], cuts[2], cuts[3]),
    xmax = c(cuts[2], cuts[3], max(ato_data$F, na.rm=TRUE)),
    ymin = c(band$T1["lo"], band$T2["lo"], band$T3["lo"]),
    ymax = c(band$T1["hi"], band$T2["hi"], band$T3["hi"]),
    label = c("T1 band", "T2 band", "T3 band")
  )

  # Envelope curve: smooth spline through tier band midpoints
  env_knots <- tibble::tibble(
    x = c(0,
          cuts[1] * 0.5,
          cuts[1],
          (cuts[1] + cuts[2]) / 2,
          cuts[2],
          (cuts[2] + cuts[3]) / 2,
          cuts[3],
          (cuts[3] + max(ato_data$F, na.rm=TRUE)) / 2,
          max(ato_data$F, na.rm=TRUE)),
    y = c(d_T0, d_T0,
          d_T0 * 0.55,
          band_mid["T1"],
          (band_mid["T1"] + band_mid["T2"]) / 2,
          band_mid["T2"],
          (band_mid["T2"] + band_mid["T3"]) / 2,
          band_mid["T3"],
          band_mid["T3"] * 0.5)
  )
  env_spline <- spline(env_knots$x, env_knots$y, n = 200, method = "natural")
  env_curve <- tibble::tibble(x = env_spline$x, y = pmax(env_spline$y, 0))

  p1 <- ggplot2::ggplot(ato_data %>% dplyr::sample_n(min(8000, nrow(ato_data))),
               ggplot2::aes(x=F, y=d_implied)) +
    ggplot2::geom_rect(data=band_rects, inherit.aes=FALSE,
              ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill="#3498db", alpha=0.10) +
    ggplot2::annotate("rect", xmin=0, xmax=cuts[1], ymin=0.61, ymax=0.64,
             fill="#27ae60", alpha=0.15) +
    ggplot2::geom_line(data=env_curve, ggplot2::aes(x=x, y=y), inherit.aes=FALSE,
              colour="#e74c3c", linewidth=1.8, alpha=0.25, linetype="solid") +
    ggplot2::geom_point(alpha=0.12, size=0.8, colour="#2c3e50") +
    ggplot2::geom_hline(yintercept=c(d_T0, band$T1["hi"], band$T1["lo"],
                           band$T2["hi"], band$T2["lo"],
                           band$T3["hi"], band$T3["lo"]),
               linetype="dotted", colour="grey60", alpha=0.4) +
    ggplot2::geom_vline(xintercept=cuts, linetype="dashed", colour="firebrick", alpha=0.6) +
    ggplot2::annotate("text", x=cuts, y=0.66,
             label=c("T0|T1","T1|T2","T2|T3"),
             hjust=-0.1, size=3, colour="firebrick") +
    ggplot2::annotate("text", x=0.01, y=d_T0+0.015, label="T0=62.5%",
             hjust=0, size=2.5, colour="#27ae60") +
    ggplot2::annotate("text", x=band_rects$xmin+0.01,
             y=(band_rects$ymin+band_rects$ymax)/2,
             label=band_rects$label, hjust=0, size=2.5, colour="#2980b9") +
    ggplot2::annotate("text", x=0.18, y=0.44,
             label="Envelope (conceptual)", hjust=0, size=2.5,
             colour="#e74c3c", alpha=0.6, fontface="italic") +
    ggplot2::labs(title="Implied discount d(F) vs composite index F (banded taper)",
         subtitle=sprintf("Curvature p=%.2f | Taper \u03b4=%.3f | Bands: T1[%.0f-%.0f%%] T2[%.0f-%.0f%%] T3[%.0f-%.0f%%]",
                           p_param, delta,
                           100*band$T1["lo"], 100*band$T1["hi"],
                           100*band$T2["lo"], 100*band$T2["hi"],
                           100*band$T3["lo"], 100*band$T3["hi"]),
         x="Composite investor index F", y="Implied discount rate d(F)") +
    ggplot2::theme_minimal()

  # --- Plot 2: F distribution by tier ---
  p2 <- ggplot2::ggplot(ato_data, ggplot2::aes(x=F, fill=tier)) +
    ggplot2::geom_histogram(bins=100, alpha=0.85) +
    ggplot2::geom_vline(xintercept=cuts, linetype="dashed", linewidth=0.5) +
    ggplot2::scale_fill_manual(values=tier_cols) +
    ggplot2::labs(title="Distribution of composite index F with tier boundaries",
         x="Composite investor index F", y="Count", fill="Tier") +
    ggplot2::theme_minimal()

  # --- Plot 3: Boundary inequity (T1|T2 and T2|T3 only) ---
  ineq_band <- 0.02
  ineq <- tibble::tibble(
    cut = cuts[2:3],
    boundary = factor(c("T1 | T2", "T2 | T3"),
                      levels=c("T1 | T2", "T2 | T3"))
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
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
    dplyr::ungroup()

  p3 <- ggplot2::ggplot(ineq, ggplot2::aes(x=boundary, y=gap)) +
    ggplot2::geom_col(fill="#8e44ad", width=0.5) +
    ggplot2::geom_text(ggplot2::aes(label=paste0(round(gap*100,1),"pp")), vjust=-0.5, size=3.5) +
    ggplot2::labs(title="Horizontal inequity at tier boundaries (with smoothstep taper)",
         subtitle=sprintf("Taper \u03b4=%.3f | Only T1|T2 and T2|T3 (T0|T1 is rule boundary)", delta),
         x="Tier boundary", y="Discount gap (percentage points)") +
    ggplot2::theme_minimal()

  # --- Plot 4: Income vs portfolio by tier ---
  p4 <- ggplot2::ggplot(ato_data %>% dplyr::sample_n(min(10000, nrow(ato_data))),
               ggplot2::aes(x=income_taxable/1000, y=portfolio_count, colour=tier)) +
    ggplot2::geom_jitter(alpha=0.5, size=1.5, height=0.2) +
    ggplot2::scale_colour_manual(values=tier_cols) +
    ggplot2::scale_x_log10(labels=scales::comma_format(suffix="k")) +
    ggplot2::labs(title="Family taxable income vs portfolio size by tier",
         x="Family taxable income ($k, log scale)", y="Investment properties",
         colour="Tier") +
    ggplot2::theme_minimal()

  # --- Plot 5: Holding period vs leverage by tier ---
  p5 <- ggplot2::ggplot(ato_data %>% dplyr::sample_n(min(10000, nrow(ato_data))),
               ggplot2::aes(x=hold_years_wavg, y=leverage_ratio, colour=tier)) +
    ggplot2::geom_point(alpha=0.5, size=1.5) +
    ggplot2::scale_colour_manual(values=tier_cols) +
    ggplot2::labs(title="Holding period vs leverage ratio by tier",
         x="Weighted avg holding period (years)",
         y="Leverage ratio (interest/rent)",
         colour="Tier") +
    ggplot2::theme_minimal()

  # --- Plot 6: Tier profile comparison ---
  tier_profile <- ato_data %>%
    dplyr::group_by(tier) %>%
    dplyr::summarise(
      income_med  = median(income_taxable)/1000,
      portfolio   = mean(portfolio_count),
      value_med   = median(property_value_total)/100000,
      leverage    = mean(leverage_ratio),
      hold        = mean(hold_years_wavg),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(-tier, names_to="metric", values_to="value")

  p6 <- ggplot2::ggplot(tier_profile, ggplot2::aes(x=metric, y=value, fill=tier)) +
    ggplot2::geom_col(position="dodge") +
    ggplot2::scale_fill_manual(values=tier_cols) +
    ggplot2::facet_wrap(~metric, scales="free", nrow=1) +
    ggplot2::labs(title="Tier profiles: how investor characteristics differ across tiers",
         x=NULL, y=NULL, fill="Tier") +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_text(face="bold"))

  list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6)
}
