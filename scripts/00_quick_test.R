## 00_quick_test.R
## Quick test (about 1 min): runs the four HCRs in the base model with 20
## replicates and compares the results with the expected values.
## Run it before the full analysis (scripts 01 to 05).

source("scripts/00_setup.R")
source("check/check_functions.R")

t0 <- Sys.time()
expected <- read.csv("check/expected_results.csv")
compare_values(quick_values(), expected[expected$check == "quick", ])
cat("00_quick_test done:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
