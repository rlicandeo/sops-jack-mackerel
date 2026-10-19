# Designing Harvest Control Rules for Jack Mackerel using Stochastic Optimization in Policy Space

Code, input data and outputs for:

> Licandeo, R., Canales, C. M., & Walters, C. *Designing Harvest Control Rules for Jack Mackerel using Stochastic Optimization in Policy Space*. Ecological Modelling (under review).

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23469253.svg)](https://doi.org/10.5281/zenodo.23469253)

## How to cite

Please cite the paper and this code:

> Licandeo, R., Canales, C. M., & Walters, C. (2026). *Code and data for: Designing Harvest Control Rules for Jack Mackerel using Stochastic Optimization in Policy Space* (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.23469253

The DOI above refers to version 1.0.0, the version used in the paper. The DOI https://doi.org/10.5281/zenodo.23469252 always resolves to the latest version.

The code runs an age-structured management strategy evaluation (MSE) for jack mackerel (*Trachurus murphyi*). It applies Stochastic Optimization in Policy Space (SOPS) through a grid search over a linear harvest control rule (HCR3). HCR3 is then compared with three traditional HCRs (HCR0 to HCR2) under three operating models. All scripts reproduce the results, tables and figures of the paper exactly. The code follows the tidyverse style (formatted with the styler package).

## Structure

```
data/      numbers_at_age.csv        JJM stock assessment output (inputs, see Data)
           F_at_age.csv
           biology_at_age.csv
           catch.csv
R/         operating_model.R         run_sops_mse(): operating model, assessment emulation, HCR0 to HCR3
           helpers.R                 yield at F, TAC stabilizer, per-recruit reference points
           selection.R               risk-neutral and HARA optima, acceptability constraints
           plots.R                   figure functions (Figures 1 to 4, Appendix Figures A1 to A4)
scripts/   00_setup.R                packages, data, settings (sourced by all scripts)
           00_quick_test.R           quick test (about 1 min) before the full analysis
           01_grid_search.R          SOPS grid search (441 HCRs x 3 operating models)
           02_hcr_comparison.R       four HCRs in the base model, traditional HCRs in all models
           03_grid_seeds.R           refined grid and independent random seeds (Appendix)
           04_figures.R              Figures 1 to 4
           05_appendix_figures.R     Appendix Figures A1 to A4
           06_check_results.R        compares your results with those of the paper
check/     expected values of the paper and functions used by the checks
outputs/   results (.rds, .csv), so that results can be checked without re-running the simulations
figures/   figures of the paper and the Appendix
renv.lock  exact versions of R and of all packages used
```

## How to reproduce the results (step by step)

**1. Install R.** The results were produced with R 4.4.2 (<https://cran.r-project.org>). Other 4.x versions should also work.

**2. Download this repository** and unzip it. All steps below are run from this folder (the one that contains `data/`, `R/` and `scripts/`). In R, set it as the working directory, for example `setwd("C:/path/to/folder")`, or open the folder in RStudio.

**3. Install the packages, with the same versions used for the paper.** The first time R opens in this folder, `renv` installs itself. Then run:

```r
renv::restore()
```

This installs the exact versions listed in `renv.lock` in a library of this project only, so the packages you already have are not changed. It takes a few minutes. On Linux, two packages need system libraries: `sf` (a dependency of `metR`, used for contour labels) and `ragg` (used to save the figures). On Ubuntu, for example: `sudo apt install libgdal-dev libgeos-dev libproj-dev libudunits2-dev libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev libpng-dev libtiff5-dev libjpeg-dev`. On Windows and macOS no extra step is needed.

**4. Quick test (about 1 minute).** Checks that everything works before the full analysis:

```r
source("scripts/00_quick_test.R")
```

It should end with `RESULT: all values are identical to the expected values.`

**5. Full analysis.** Run the scripts in order:

```r
source("scripts/01_grid_search.R")      # about 3 min with 31 cores
source("scripts/02_hcr_comparison.R")   # about 1 min
source("scripts/03_grid_seeds.R")       # about 1 min
source("scripts/04_figures.R")
source("scripts/05_appendix_figures.R")
```

Each simulation uses 500 replicates and a 50-year projection. Scripts 01 to 03 run in parallel on all cores but one (`n_cores` in `00_setup.R`). With fewer cores, the run time increases roughly in proportion (e.g., about 1 hour with 4 cores). Scripts 04 and 05 only read the outputs, so they can be run without re-running the simulations.

**6. Check your results against the paper:**

```r
source("scripts/06_check_results.R")
```

It compares 140 values (reference points, Tables 2, 3 and A1, and the risk-neutral and HARA optima) and reports one of three results:

- `all values are identical to the expected values`: the paper is reproduced exactly.
- `OK. Minor numerical differences`: small differences in the last decimals, which can occur with another operating system or R version (relative difference below 1%, or below 0.005 for probabilities). The conclusions do not change.
- `some values differ`: check that `renv::restore()` was run and that all scripts were run in order.

Note that scripts 01 to 05 overwrite the files in `outputs/` and `figures/`. The expected values of the paper are kept in `check/expected_results.csv`.

## Where each result is

| Paper | Output |
|---|---|
| Reference points (Section 2.2) | `outputs/reference_points.csv` |
| Figure 1 (policy space) | `figures/Figure1_policy_space.png`, from `outputs/sops_results_all.rds` |
| Figure 2 (projected dynamics) | `figures/Figure2_trajectories.png`, `outputs/HCR_trajectory_summary.csv` |
| Figure 3 (F versus SSB) | `figures/Figure3_HCR_shape.png` |
| Figure 4 (Pareto frontier) | `figures/Figure4_pareto.png`, `outputs/sops_HCRs_under_scenarios_pareto.csv` |
| Table 2 (acceptable HCRs) | `outputs/acceptable_HCRs_M0.csv`, `outputs/risk_averse_optimum_M0.csv` |
| Table 3 (HCRs across operating models) | `outputs/traditional_HCRs_under_scenarios.csv`, `outputs/sops_HCRs_under_scenarios_pareto.csv` |
| Appendix Figures A1 to A3 | `figures/FigureA1_*.png` to `figures/FigureA3_*.png` |
| Appendix Tables A1, A2 and Figure A4 | `outputs/appendix_*.csv`, `figures/FigureA4_refined_grid.png` |

## Reproducibility

The random seed is fixed for each replicate (`set.seed(i + seed_offset)` in `run_sops_mse()`). Thus, all HCRs are evaluated under the same stochastic sequences (recruitment, selectivity, assessment and implementation errors). With `seed_offset = 0` (the default), the scripts reproduce the published results exactly. Script 03 uses other values of `seed_offset` to test sensitivity to the random seed.

## Requirements

Tested with R 4.4.2 and the packages dplyr 1.2.0, ggplot2 4.0.2, metR 0.18.2, patchwork 1.3.2, ragg 1.5.1 and scales 1.4.0 (plus parallel, which is included in R). The exact versions of all packages, including dependencies, are in `renv.lock` (step 3 above). Without `renv`, the packages can also be installed with:

```r
install.packages(c("dplyr", "ggplot2", "metR", "patchwork", "ragg", "scales"))
```

## Data

The files in `data/` were extracted from the output of the Joint Jack Mackerel (JJM) stock assessment model of the South Pacific Regional Fisheries Management Organisation (SPRFMO), 1970 to 2024. They condition the operating model.

| File | Content | Units |
|---|---|---|
| `numbers_at_age.csv` | numbers at age (ages 1 to 12, age 12 is a plus group), by year | millions of fish |
| `F_at_age.csv` | fishing mortality at age, by year (selectivity is derived from it) | year⁻¹ |
| `biology_at_age.csv` | weight, maturity and natural mortality at age | kg, proportion, year⁻¹ |
| `catch.csv` | observed catch by fleet and total, by year (only the 2024 total is used, as the initial catch of the projections) | thousand tonnes (kt) |

Numbers at age (millions) times weight at age (kg) gives biomass in kt, the unit of all catches and biomasses in the code.

SPRFMO (2024). SC12 Report: Annex 7. Jack Mackerel Technical Annex. Scientific Committee Report. South Pacific Regional Fisheries Management Organisation, Wellington, New Zealand.

## Version history

This version corrects three errors in the code used for the originally submitted manuscript. All results, tables and figures were re-run, and the revised manuscript reports the corrected values.

1. Recruitment was computed from the estimated SSB (with assessment error and bias) instead of the true SSB of the previous year.
2. The initial catch and TAC of the projections were set to the year of the last assessment (2024) instead of the observed 2024 catch (1,265 kt). This affected the 15% stabilizer of HCR0 and HCR2.
3. Unfished spawning biomass (B0) was computed at F = 0.001 instead of F = 0 (0.5% lower).

In addition, the 5% of B0 rule of HCR0 and HCR2 now takes precedence over the other zones, as in the manuscript equations, and the reference points shown in Figure 2 are read from the model output (`outputs/reference_points.csv`). The corrected model was checked against an independent implementation of the manuscript equations, replicate by replicate.

## Variable names

Some argument names are in Spanish: `regla` (rule), `CBA` (total allowable catch, TAC), `sesgoEst` (assessment bias), `sesgoY` (implementation bias), `banda` (stabilizer band), `BminH` (Bmin).

## License

MIT License (see `LICENSE`). The input data in `data/` were extracted from the SPRFMO jack mackerel stock assessment (SPRFMO 2024, see Data); please cite that source when using them.

## Contact

Roberto Licandeo, IMFEMC Consulting, Chilliwack, BC, Canada.
