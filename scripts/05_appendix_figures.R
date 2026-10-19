## 05_appendix_figures.R
## Appendix Figures A1 to A4 (requires scripts 01 and 03).
## Outputs: figures/FigureA1_HARA_relaxed_dep.png, figures/FigureA2_HARA_relaxed_dep_AAV.png,
##          figures/FigureA3_Carruthers.png, figures/FigureA4_refined_grid.png

source("scripts/00_setup.R")
results_all <- readRDS("outputs/sops_results_all.rds")
A <- read.csv("outputs/appendix_refined_grid_M0.csv")

in_dir("figures", {
  ## A1: depletion constraint relaxed (P(SSB < SSBmsy) <= 0.50), AAV <= 0.30
  plot_utility(
    results_all = results_all, filter_scenario = "M0", show_safe = TRUE,
    save_grid = FALSE, max_prob_below_SSBmsy = 0.5,
    max_prob_above_Fmsy = 0.5, max_catch_avv = 0.3
  )
  file.rename("SOPS_utility_contour_M0.png", "FigureA1_HARA_relaxed_dep.png")
  ## A2: as A1, with AAV <= 0.50
  plot_utility(
    results_all = results_all, filter_scenario = "M0", show_safe = TRUE,
    save_grid = FALSE, max_prob_below_SSBmsy = 0.5,
    max_prob_above_Fmsy = 0.5, max_catch_avv = 0.5
  )
  file.rename("SOPS_utility_contour_M0.png", "FigureA2_HARA_relaxed_dep_AAV.png")
  ## A3: Carruthers et al. (2016) criteria with AAV < 0.50
  plot_SSBmsy_carruthers(
    results_all = results_all, filter_scenario = "M0",
    min_prob_above_half_SSBmsy = 0.60, min_prob_below_125Fmsy = 0.60,
    max_catch_avv = 0.5, min_mean_catch = 0,
    show_values = FALSE, show_safe = TRUE, save_grid = FALSE
  )
  file.rename("SOPS_above_half_SSBmsy_contour_M0.png", "FigureA3_Carruthers.png")
})

## A4: refined grid around the acceptable region
plot_refined_grid(A, file = "figures/FigureA4_refined_grid.png")

cat("05 done\n")
