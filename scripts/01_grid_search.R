## 01_grid_search.R
## SOPS grid search: 441 HCR3 parameter combinations x 3 operating models.
## Outputs: outputs/sops_results_all.rds, risk-neutral and HARA optima,
##          acceptable HCRs of the base model.

source("scripts/00_setup.R")
t0 <- Sys.time()

grid <- expand.grid(
  Qslope = seq(0.00, 1.0, by = 0.05),
  BminH = seq(0.00, 20000, by = 1000)
)

cl <- sops_cluster()
results_all <- do.call(rbind, lapply(scenarios, function(sc) {
  out <- parLapply(cl, split(grid, seq_len(nrow(grid))), function(g, sc) {
    res <- do.call(run_sops_mse, c(list(
      Qslope = g$Qslope, BminH = g$BminH,
      Nrepl = Nrepl, years = years
    ), sc))
    data.frame(
      scenario_id            = sc$scenario_id,
      Qslope                 = res$Qslope,
      BminH                  = res$BminH,
      mean_catch             = res$mean_catch,
      mean_utility           = res$mean_utility,
      sd_utility             = res$sd_utility,
      prob_below_SSBmsy      = res$prob_below_SSBmsy,
      prob_above_half_SSBmsy = res$prob_above_half_SSBmsy,
      prob_above_Fmsy        = res$prob_above_Fmsy,
      prob_below_125Fmsy     = res$prob_below_125Fmsy,
      prob_above_msy         = res$prob_above_msy,
      catch_cv               = res$catch_cv,
      catch_avv              = res$catch_avv
    )
  }, sc = sc)
  do.call(rbind, out)
}))
stopCluster(cl)
saveRDS(results_all, "outputs/sops_results_all.rds")

## Risk-neutral (max mean catch) and HARA (max utility) optima, base model
find_optimal_hcr(results_all, scenario = "M0", save = TRUE, save_dir = "outputs")

## Acceptable HCRs of the base model (Table 2) and the selected HCR
acc <- results_all %>%
  filter(scenario_id == "M0", Qslope > 0) %>%
  filter(is_acceptable(.)) %>%
  arrange(desc(mean_catch))
write.csv(acc, "outputs/acceptable_HCRs_M0.csv", row.names = FALSE)
write.csv(acc %>% filter(abs(Qslope - sel_Qslope) < 1e-9, abs(BminH - sel_BminH) < 1e-9),
  "outputs/selected_HCR_M0.csv",
  row.names = FALSE
)

cat(
  "01 done:", nrow(results_all), "runs,", nrow(acc), "acceptable HCRs in M0,",
  round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n"
)
