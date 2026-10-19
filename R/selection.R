## selection.R
## Risk-neutral (maximum mean catch) and HARA (maximum utility) optima, and the
## acceptability constraints.

find_optimal_hcr <- function(results_df, scenario = "M0", qslope_min = 0, save = FALSE, save_dir = ".") {
  # Filter
  grid <- results_df %>%
    filter(
      scenario_id == scenario,
      Qslope > qslope_min
    )

  # Risk-neutral optimum (max mean catch)
  best_mean <- grid[which.max(grid$mean_catch), ]

  # Risk-averse optimum (max HARA utility)
  best_utility <- grid[which.max(grid$mean_utility), ]

  # Print results
  cat("\n=== RISK-NEUTRAL (Max Mean Catch) ===\n")
  cat(
    "Qslope =", best_mean$Qslope, ", BminH =", best_mean$BminH,
    ", Mean Catch =", round(best_mean$mean_catch),
    ", Utility =", round(best_mean$mean_utility, 2), "\n"
  )

  cat("\n=== RISK-AVERSE (Max HARA Utility C^0.6) ===\n")
  cat(
    "Qslope =", best_utility$Qslope, ", BminH =", best_utility$BminH,
    ", Mean Catch =", round(best_utility$mean_catch),
    ", Utility =", round(best_utility$mean_utility, 2), "\n"
  )

  # Optionally save
  if (save) {
    write.csv(best_mean,
      file.path(save_dir, paste0("risk_neutral_optimum_", scenario, ".csv")),
      row.names = FALSE
    )
    write.csv(best_utility,
      file.path(save_dir, paste0("risk_averse_optimum_", scenario, ".csv")),
      row.names = FALSE
    )
    cat("\nFiles saved to:", save_dir, "\n")
  }

  invisible(list(
    risk_neutral = best_mean,
    risk_averse  = best_utility
  ))
}


## Acceptable HCRs: meet the three precautionary constraints of the paper
## (P(SSB < SSBmsy) <= 0.40, P(F > Fmsy) < 0.50, AAV < 0.30).
is_acceptable <- function(d, max_dep = 0.40, max_ovf = 0.50, max_aav = 0.30) {
  d$prob_below_SSBmsy <= max_dep & d$prob_above_Fmsy < max_ovf & d$catch_avv < max_aav
}
