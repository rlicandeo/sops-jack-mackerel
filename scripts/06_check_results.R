## 06_check_results.R
## Compares your results (outputs/, after running scripts 01 to 05) with the key
## results of the paper: reference points, Tables 2, 3 and A1, and the
## risk-neutral and HARA optima.

source("scripts/00_setup.R")
source("check/check_functions.R")

expected <- read.csv("check/expected_results.csv")
compare_values(full_values(), expected[expected$check == "full", ])
