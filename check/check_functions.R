## check_functions.R
## Functions used by scripts/00_quick_test.R, scripts/06_check_results.R and
## check/make_expected.R.

metric_names <- c("mean_catch", "prob_below_SSBmsy", "prob_above_Fmsy", "prob_above_msy", "catch_avv")

## Long format: one row per value
as_long <- function(d, check, table, item) {
  do.call(rbind, lapply(metric_names, function(m) {
    data.frame(check = check, table = table, item = item, metric = m, value = d[[m]])
  }))
}

## Quick test: the four HCRs in the base model with 20 replicates
quick_values <- function() {
  hcrs <- list(
    HCR0 = list(regla = 0, Q = NA, B = NA), HCR1 = list(regla = 1, Q = NA, B = NA),
    HCR2 = list(regla = 2, Q = NA, B = NA), HCR3 = list(regla = 3, Q = sel_Qslope, B = sel_BminH)
  )
  do.call(rbind, lapply(names(hcrs), function(h) {
    x <- hcrs[[h]]
    res <- run_sops_mse(Qslope = x$Q, BminH = x$B, Nrepl = 20, years = years, regla = x$regla)
    as_long(res, "quick", "quick_test_20_replicates", h)
  }))
}

## Full check: key results of the paper, read from outputs/
full_values <- function() {
  ref <- read.csv("outputs/reference_points.csv")
  out <- data.frame(
    check = "full", table = "reference_points", item = "base",
    metric = names(ref), value = unlist(ref[1, ])
  )
  trad <- read.csv("outputs/traditional_HCRs_under_scenarios.csv")
  for (i in seq_len(nrow(trad))) {
    out <- rbind(out, as_long(trad[i, ], "full", "Table3", paste0(trad$scenario_id[i], "_HCR", trad$regla[i])))
  }
  sops <- read.csv("outputs/sops_HCRs_under_scenarios_pareto.csv")
  sops <- sops[abs(sops$Qslope - sel_Qslope) < 1e-9 & abs(sops$BminH - sel_BminH) < 1e-9, ]
  for (i in seq_len(nrow(sops))) {
    out <- rbind(out, as_long(sops[i, ], "full", "Table3", paste0(sops$scenario_id[i], "_HCR3")))
  }
  acc <- read.csv("outputs/acceptable_HCRs_M0.csv")
  for (i in seq_len(nrow(acc))) {
    out <- rbind(out, as_long(acc[i, ], "full", "Table2", sprintf("Qslope%.2f_Bmin%d", acc$Qslope[i], acc$BminH[i])))
  }
  for (f in c("risk_neutral", "risk_averse")) {
    o <- read.csv(sprintf("outputs/%s_optimum_M0.csv", f))
    out <- rbind(out, data.frame(
      check = "full", table = "optima", item = f,
      metric = c("Qslope", "BminH", "mean_catch"), value = c(o$Qslope, o$BminH, o$mean_catch)
    ))
  }
  seeds <- read.csv("outputs/appendix_seed_sets_M0.csv")
  seeds <- seeds[abs(seeds$Qslope - sel_Qslope) < 1e-9 & abs(seeds$BminH - sel_BminH) < 1e-9, ]
  for (i in seq_len(nrow(seeds))) {
    out <- rbind(out, as_long(seeds[i, ], "full", "TableA1", paste0("seed_offset_", seeds$seed_offset[i])))
  }
  rownames(out) <- NULL
  out
}

## Compares obtained with expected values and prints a summary
compare_values <- function(obtained, expected) {
  m <- merge(expected, obtained, by = c("check", "table", "item", "metric"), suffixes = c("_paper", "_yours"), all.x = TRUE)
  m$abs_diff <- abs(m$value_yours - m$value_paper)
  m$rel_diff <- m$abs_diff / pmax(abs(m$value_paper), 1e-12)
  m$status <- ifelse(is.na(m$value_yours), "MISSING",
    ifelse(m$rel_diff < 1e-6 | m$abs_diff < 1e-9, "identical",
      ifelse(m$rel_diff < 0.01 | m$abs_diff < 0.005, "minor difference", "DIFFERENT")
    )
  )
  cat("\nValues checked:", nrow(m), "\n")
  print(table(m$status))
  bad <- m[m$status != "identical", c("table", "item", "metric", "value_paper", "value_yours", "status")]
  if (nrow(bad) == 0) {
    cat("\nRESULT: all values are identical to the expected values.\n")
  } else if (all(bad$status == "minor difference")) {
    cat("\nRESULT: OK. Minor numerical differences (e.g., another operating system or R version);",
        "the conclusions do not change.\n")
    print(head(bad, 20), row.names = FALSE)
  } else {
    cat("\nRESULT: some values differ from the expected values. Check the R and package versions",
        "(renv::restore()) and that all scripts were run in order.\n")
    print(bad[bad$status %in% c("DIFFERENT", "MISSING"), ], row.names = FALSE)
  }
  invisible(m)
}
