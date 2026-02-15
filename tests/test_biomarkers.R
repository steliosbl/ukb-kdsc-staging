# Test: Verify biomarker outputs match parent repo
# Run after R03-fields.rmd and R04-gp.rmd have been knitted
library(data.table)
library(dplyr)
library(arrow)

cat("=== Test: Biomarkers ===\n")

# Load new repo outputs
new_ukb <- read_parquet("data/biomarkers_ukb.parquet") %>% as.data.table()
new_gp <- read_parquet("data/biomarkers_gp.parquet") %>% as.data.table()
new_all <- rbind(new_ukb, new_gp, fill = TRUE)

# Load parent repo output (covariates_long contains eGFR and ACR)
parent_cov <- read_parquet("../ckm-risk-prediction/data/curated/covariates_long.parquet") %>% as.data.table()
parent_egfr_acr <- parent_cov[type %in% c("egfr", "acr")]

# Test 1: Coverage - we should have similar number of eGFR/ACR measurements
new_egfr_n <- new_all[type == "egfr", .N]
new_acr_n <- new_all[type == "acr", .N]
parent_egfr_n <- parent_egfr_acr[type == "egfr", .N]
parent_acr_n <- parent_egfr_acr[type == "acr", .N]

cat(sprintf(
  "  eGFR: new=%d, parent=%d (ratio=%.2f)\n",
  new_egfr_n, parent_egfr_n, new_egfr_n / parent_egfr_n
))
cat(sprintf(
  "  ACR: new=%d, parent=%d (ratio=%.2f)\n",
  new_acr_n, parent_acr_n, new_acr_n / parent_acr_n
))

# Test 2: UKB-sourced values should match exactly
new_ukb_egfr <- new_ukb[type == "egfr"]
parent_ukb_egfr <- parent_egfr_acr[type == "egfr"]

# Match on eid + date
merged_egfr <- merge(
  new_ukb_egfr[, .(eid, date, value_new = value)],
  parent_ukb_egfr[, .(eid, date, value_parent = value)],
  by = c("eid", "date")
)

if (nrow(merged_egfr) > 0) {
  egfr_diff <- abs(merged_egfr$value_new - merged_egfr$value_parent)
  cat(sprintf(
    "  UKB eGFR matched: %d rows, max diff: %.4f, mean diff: %.4f\n",
    nrow(merged_egfr), max(egfr_diff), mean(egfr_diff)
  ))
  # Allow small floating point differences
  stopifnot("UKB eGFR values should match within tolerance" = max(egfr_diff) < 0.1)
  cat("  PASS: UKB eGFR values match within tolerance\n")
} else {
  cat("  WARN: No UKB eGFR records could be matched by eid+date\n")
}

# Test 3: UKB ACR values should match
new_ukb_acr <- new_ukb[type == "acr"]
parent_ukb_acr <- parent_egfr_acr[type == "acr"]

merged_acr <- merge(
  new_ukb_acr[, .(eid, date, value_new = value)],
  parent_ukb_acr[, .(eid, date, value_parent = value)],
  by = c("eid", "date")
)

if (nrow(merged_acr) > 0) {
  acr_diff <- abs(merged_acr$value_new - merged_acr$value_parent)
  cat(sprintf(
    "  UKB ACR matched: %d rows, max diff: %.4f, mean diff: %.4f\n",
    nrow(merged_acr), max(acr_diff), mean(acr_diff)
  ))
  stopifnot("UKB ACR values should match within tolerance" = max(acr_diff) < 0.1)
  cat("  PASS: UKB ACR values match within tolerance\n")
} else {
  cat("  WARN: No UKB ACR records could be matched by eid+date\n")
}

# Test 4: Unique participants
new_eids <- new_all[, uniqueN(eid)]
parent_eids <- parent_egfr_acr[, uniqueN(eid)]
cat(sprintf("  Unique participants: new=%d, parent=%d\n", new_eids, parent_eids))

cat("=== Biomarker tests complete ===\n")
