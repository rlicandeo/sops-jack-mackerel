## make_expected.R
## Writes check/expected_results.csv: the values reported in the paper, used by
## scripts/00_quick_test.R and scripts/06_check_results.R. Run by the authors
## from the repository root after scripts 01 to 05. Users do not need to run it.

source("scripts/00_setup.R")
source("check/check_functions.R")

expected <- rbind(
  quick_values(), # quick test: 20 replicates, four HCRs, base model
  full_values() # key results of the paper, read from outputs/
)
write.csv(expected, "check/expected_results.csv", row.names = FALSE)
cat("check/expected_results.csv written:", nrow(expected), "values\n")
