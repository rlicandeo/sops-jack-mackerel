## 03_grid_seeds.R
## Appendix section 2 (revision, R4.5): grid resolution and random seed sensitivity,
## base model.
##   A. Refined grid: Qslope 0.01 to 0.25 (step 0.01) x Bmin 0 to 3,000 kt (step 250).
##   B. Independent seed sets: original grid cells around the acceptable region
##      (Qslope 0.05 to 0.30, Bmin 0 to 4,000 kt) with seed_offset > 0.
##   C. The eight highest-yield acceptable HCRs of the refined grid with the same seed sets.
## seed_offset = 0 reproduces the published results.
## Outputs: outputs/appendix_refined_grid_M0.csv, outputs/appendix_seed_sets_M0.csv,
##          outputs/appendix_edge_seed_sets_M0.csv

source("scripts/00_setup.R")
t0 <- Sys.time()
seed_sets <- c(0, 1e4, 2e4, 3e4, 4e4, 5e4) # far apart, so replicate seeds never overlap

cl <- sops_cluster()
run_jobs <- function(jobs) {
  do.call(rbind, parLapply(cl, split(jobs, seq_len(nrow(jobs))), function(j) {
    res <- run_sops_mse(
      Qslope = j$Qslope, BminH = j$BminH, Nrepl = Nrepl,
      years = years, seed_offset = j$seed_offset
    )
    cbind(j, data.frame(
      mean_catch = res$mean_catch, mean_utility = res$mean_utility,
      prob_below_SSBmsy = res$prob_below_SSBmsy,
      prob_above_Fmsy = res$prob_above_Fmsy,
      prob_above_msy = res$prob_above_msy, catch_avv = res$catch_avv
    ))
  }))
}

## A. refined grid
A <- run_jobs(expand.grid(
  Qslope = seq(0.01, 0.25, by = 0.01),
  BminH = seq(0, 3000, by = 250), seed_offset = 0
))
write.csv(A, "outputs/appendix_refined_grid_M0.csv", row.names = FALSE)

## B. independent seed sets, original grid cells
B <- run_jobs(expand.grid(
  Qslope = seq(0.05, 0.30, by = 0.05),
  BminH = seq(0, 4000, by = 1000), seed_offset = seed_sets
))
write.csv(B, "outputs/appendix_seed_sets_M0.csv", row.names = FALSE)

## C. highest-yield acceptable HCRs of the refined grid, all seed sets
top <- A %>%
  filter(is_acceptable(.)) %>%
  arrange(desc(mean_catch)) %>%
  head(8) %>%
  select(Qslope, BminH)
E <- run_jobs(merge(top, data.frame(seed_offset = seed_sets)))
write.csv(E, "outputs/appendix_edge_seed_sets_M0.csv", row.names = FALSE)
stopCluster(cl)

cat("03 done:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
