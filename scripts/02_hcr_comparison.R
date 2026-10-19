## 02_hcr_comparison.R
## Comparison of the four HCRs.
##   a) Base model, with full trajectories (Figure 2, Figure 3).
##   b) Traditional HCRs under the three operating models (Table 3, Figure 4).
## Outputs: outputs/results_hcr_M0.rds, outputs/HCR_trajectory_summary.csv,
##          outputs/all_HCRs_comparison_M0.csv, outputs/result_selected_M0.rds,
##          outputs/traditional_HCRs_under_scenarios.csv

source("scripts/00_setup.R")
t0 <- Sys.time()
cl <- sops_cluster()

## a) Base model: HCR0, HCR1, HCR2 and the selected HCR3
hcr_list <- list(
  list(regla = 0, Qslope = NA, BminH = NA),
  list(regla = 1, Qslope = NA, BminH = NA),
  list(regla = 2, Qslope = NA, BminH = NA),
  list(regla = 3, Qslope = sel_Qslope, BminH = sel_BminH)
)
results_hcr <- parLapply(cl, hcr_list, function(h) {
  res <- run_sops_mse(
    Qslope = h$Qslope, BminH = h$BminH, Nrepl = Nrepl,
    years = years, regla = h$regla
  )
  list(
    metrics = data.frame(
      regla = h$regla, mean_catch = res$mean_catch,
      catch_cv = res$catch_cv, catch_avv = res$catch_avv,
      prob_below_SSBmsy = res$prob_below_SSBmsy,
      prob_above_Fmsy = res$prob_above_Fmsy,
      prob_above_msy = res$prob_above_msy
    ),
    matB = res$matB, matY = res$matY, matF = res$matF
  )
})
saveRDS(results_hcr, "outputs/results_hcr_M0.rds")
write.csv(do.call(rbind, lapply(results_hcr, `[[`, "metrics")),
  "outputs/all_HCRs_comparison_M0.csv",
  row.names = FALSE
)

## Median and quantiles of SSB, catch and F per year and HCR
hcr_names <- c("HCR0", "HCR1", "HCR2", "HCR3")
trajectory_summary <- do.call(rbind, lapply(seq_along(results_hcr), function(i) {
  do.call(rbind, lapply(list(
    list(mat = "matB", var = "SSB"),
    list(mat = "matY", var = "Catch"),
    list(mat = "matF", var = "F")
  ), function(v) {
    mat <- results_hcr[[i]][[v$mat]]
    data.frame(
      HCR = hcr_names[i], Var = v$var, Year = 1:years,
      Median = round(apply(mat, 2, median), 3),
      Q05 = round(apply(mat, 2, quantile, 0.05), 3),
      Q25 = round(apply(mat, 2, quantile, 0.25), 3),
      Q75 = round(apply(mat, 2, quantile, 0.75), 3),
      Q95 = round(apply(mat, 2, quantile, 0.95), 3)
    )
  }))
}))
write.csv(trajectory_summary, "outputs/HCR_trajectory_summary.csv", row.names = FALSE)

## Selected HCR with estimated biomass, for the phase diagram (Figure 3)
result <- run_sops_mse(Qslope = sel_Qslope, BminH = sel_BminH, Nrepl = Nrepl, years = years)
saveRDS(list(matBest = result$matBest, matF = result$matF), "outputs/result_selected_M0.rds")

## Baseline reference points used by the performance metrics (Section 2.2, Figure 2)
write.csv(data.frame(
  B0 = result$B0, SSBmsy = result$SSBmsy_base,
  Fmsy = result$Fmsy_base, MSY = result$MSY_base
), "outputs/reference_points.csv", row.names = FALSE)

## b) Traditional HCRs under the three operating models
combos <- unlist(lapply(scenarios, function(sc) {
  lapply(list(0, 1, 2), function(r) c(sc, list(regla = r, Qslope = NA, BminH = NA)))
}), recursive = FALSE)
trad <- do.call(rbind, parLapply(cl, combos, function(x) {
  res <- run_sops_mse(
    Qslope = x$Qslope, BminH = x$BminH, Nrepl = Nrepl, years = years,
    regla = x$regla, sigmaEst = x$sigmaEst, rho = x$rho,
    scenario_id = x$scenario_id
  )
  data.frame(
    scenario_id = x$scenario_id, regla = x$regla, mean_catch = res$mean_catch,
    catch_cv = res$catch_cv, catch_avv = res$catch_avv,
    prob_below_SSBmsy = res$prob_below_SSBmsy,
    prob_above_Fmsy = res$prob_above_Fmsy, prob_above_msy = res$prob_above_msy
  )
}))
stopCluster(cl)
write.csv(trad, "outputs/traditional_HCRs_under_scenarios.csv", row.names = FALSE)

cat("02 done:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
