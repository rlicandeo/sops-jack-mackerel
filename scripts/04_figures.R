## 04_figures.R
## Figures 1 to 4 of the manuscript (requires scripts 01 and 02).
## Outputs: figures/Figure1_policy_space.png, figures/Figure2_trajectories.png,
##          figures/Figure3_HCR_shape.png, figures/Figure4_pareto.png,
##          outputs/sops_HCRs_under_scenarios_pareto.csv

source("scripts/00_setup.R")
results_all <- readRDS("outputs/sops_results_all.rds")
results_hcr <- readRDS("outputs/results_hcr_M0.rds")
result <- readRDS("outputs/result_selected_M0.rds")
trad <- read.csv("outputs/traditional_HCRs_under_scenarios.csv")

## Figure 1: policy space of HCR3 in the base model
plot_policy_space(results_all, scenario = "M0", file = "figures/Figure1_policy_space.png")

## Figure 2: projected SSB, catch and F under the four HCRs
ref <- read.csv("outputs/reference_points.csv")
plot_trajectories(results_hcr,
  SSBmsy = ref$SSBmsy, MSY = ref$MSY, Fmsy = ref$Fmsy,
  file = "figures/Figure2_trajectories.png"
)

## Figure 3: realized F versus SSB under the selected HCR
plot_hcr_F(
  matB = result$matBest, matF = result$matF, Qslope = sel_Qslope, BminH = sel_BminH,
  Bpow = 1, SSBmsy_base = NA, Fmsy_base = NA,
  save_path = "figures/Figure3_HCR_shape.png"
)

## Figure 4: Pareto frontier across the three operating models
out <- in_dir("figures", plot_tradeoff(results_all,
  filter_scenario   = c("M0", "M0_lowRho", "M0_lowRho_lowSigmaEst"),
  hcr_metrics       = trad,
  vline_prob_SSBmsy = 0.40,
  return_data       = TRUE
))
file.rename(
  "figures/SOPS_tradeoff_pareto_M0_M0_lowRho_M0_lowRho_lowSigmaEst.png",
  "figures/Figure4_pareto.png"
)
write.csv(out$safe_hcrs, "outputs/sops_HCRs_under_scenarios_pareto.csv", row.names = FALSE)

cat("04 done\n")
