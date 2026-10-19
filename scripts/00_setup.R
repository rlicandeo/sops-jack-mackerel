## 00_setup.R
## Common setup, sourced by every script. Run all scripts from the repository root.

if (!file.exists("data/numbers_at_age.csv")) {
  stop("Run the scripts from the repository root (the folder that contains data/, R/ and scripts/).")
}

suppressPackageStartupMessages({
  library(parallel)
  library(dplyr)
  library(ggplot2)
  library(metR)
  library(patchwork)
})

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
dir.create("outputs", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

## JJM stock assessment output (SPRFMO SC12). run_sops_mse() reads the global 'data'.
naa <- read.csv("data/numbers_at_age.csv")
faa <- read.csv("data/F_at_age.csv")
bio <- read.csv("data/biology_at_age.csv")
obs_catch <- read.csv("data/catch.csv")
data <- list(
  N = unname(as.matrix(naa[, -1])), # numbers at age (millions), years x ages
  TotF = unname(as.matrix(faa[, -1])), # fishing mortality at age (year^-1), years x ages
  wt_a_pop = bio$weight_kg, # weight at age (kg)
  mature_a = bio$maturity, # maturity at age
  M = bio$M, # natural mortality at age (year^-1)
  catch = obs_catch$total_kt # observed total catch (kt)
)
rm(naa, faa, bio, obs_catch)

## Simulation settings used in the paper
Nrepl <- 500 # replicates
years <- 50 # projection years

## Operating models (Table 1)
scenarios <- list(
  list(sigmaEst = 0.20, rho = 0.77, scenario_id = "M0"),
  list(sigmaEst = 0.20, rho = 0.50, scenario_id = "M0_lowRho"),
  list(sigmaEst = 0.15, rho = 0.50, scenario_id = "M0_lowRho_lowSigmaEst")
)

## Constrained-optimal HCR (HCR3) selected in the paper
sel_Qslope <- 0.15
sel_BminH <- 1000

## Parallel cluster with everything run_sops_mse() needs
n_cores <- max(1, detectCores() - 1)
sops_cluster <- function(n = n_cores) {
  cl <- makeCluster(n)
  clusterExport(cl, c(
    "run_sops_mse", "Catch", "Estabiliza", "por_recluta_r",
    "data", "Nrepl", "years"
  ), envir = globalenv())
  invisible(clusterEvalQ(cl, library(dplyr)))
  cl
}

## Run code inside a folder (some plot functions save to the working directory)
in_dir <- function(dir, code) {
  old <- setwd(dir)
  on.exit(setwd(old))
  force(code)
}
