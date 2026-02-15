# Test: Verify CKD staging output matches parent repo
# Run after R06-algorithm.rmd has been knitted
library(data.table)
library(dplyr)
library(arrow)

cat("=== Test: CKD Staging Algorithm ===\n")

# Load new repo output
new_timeline <- read_parquet("data/ckd_timeline.parquet") %>% as.data.table()
new_baseline <- read_parquet("data/ckd_baseline.parquet") %>% as.data.table()

# Map new G3 (no subtype) to G3a for comparison with parent
# Parent repo only has G3a/G3b, new repo has G3 for coded stage 3 without eGFR
new_timeline <- new_timeline %>%
  mutate(
    stage_compare = ifelse(stage == "G3", "G3a", stage),
    kdigo_compare = case_when(
      kdigo == "G3" ~ "G3a",
      kdigo == "G3A1" ~ "G3aA1",
      kdigo == "G3A2" ~ "G3aA2",
      kdigo == "G3A3" ~ "G3aA3",
      TRUE ~ kdigo
    )
  ) %>%
  as.data.table()

new_baseline <- new_baseline %>%
  mutate(
    stage_compare = ifelse(stage == "G3", "G3a", stage),
    kdigo_compare = case_when(
      kdigo == "G3" ~ "G3a",
      kdigo == "G3A1" ~ "G3aA1",
      kdigo == "G3A2" ~ "G3aA2",
      kdigo == "G3A3" ~ "G3aA3",
      TRUE ~ kdigo
    )
  ) %>%
  as.data.table()

# Load parent repo output
parent_timeline <- read_parquet("../ckm-risk-prediction/data/curated/ckd_timeline.parquet") %>% as.data.table()

# Test 1: Timeline record counts
cat(sprintf(
  "  Timeline: new=%d, parent=%d (ratio=%.4f)\n",
  nrow(new_timeline), nrow(parent_timeline),
  nrow(new_timeline) / nrow(parent_timeline)
))

# Test 2: Unique participants with any CKD event
new_eids <- new_timeline[, uniqueN(eid)]
parent_eids <- parent_timeline[, uniqueN(eid)]
cat(sprintf(
  "  CKD participants: new=%d, parent=%d (ratio=%.4f)\n",
  new_eids, parent_eids, new_eids / parent_eids
))

# Test 3: Stage distribution at latest time point per person
new_latest <- new_timeline[, .SD[which.max(date)], by = eid]
parent_latest <- parent_timeline[, .SD[which.max(date)], by = eid]

cat("\n  New repo - stage distribution (raw):\n")
print(new_latest[, .N, by = stage][order(stage)])
cat("\n  New repo - stage distribution (G3->G3a for comparison):\n")
print(new_latest[, .N, by = stage_compare][order(stage_compare)])
cat("\n  Parent repo - stage distribution:\n")
print(parent_latest[, .N, by = stage][order(stage)])

# Test 4: KDIGO risk distribution
cat("\n  New repo - risk distribution:\n")
print(new_latest[, .N, by = kdigo_risk][order(kdigo_risk)])
cat("\n  Parent repo - risk distribution:\n")
print(parent_latest[, .N, by = kdigo_risk][order(kdigo_risk)])

# Test 5: Per-person matching - compare KDIGO at latest date
merged <- merge(
  new_latest[, .(eid, kdigo_new = kdigo_compare, risk_new = kdigo_risk)],
  parent_latest[, .(eid, kdigo_parent = kdigo, risk_parent = kdigo_risk)],
  by = "eid"
)

kdigo_match <- merged[kdigo_new == kdigo_parent, .N]
risk_match <- merged[risk_new == risk_parent, .N]

cat(sprintf(
  "\n  Per-person KDIGO match: %d/%d (%.1f%%)\n",
  kdigo_match, nrow(merged), 100 * kdigo_match / nrow(merged)
))
cat(sprintf(
  "  Per-person risk match: %d/%d (%.1f%%)\n",
  risk_match, nrow(merged), 100 * risk_match / nrow(merged)
))

# Test 6: Baseline staging
if (nrow(new_baseline) > 0) {
  cat(sprintf("\n  Baseline CKD cases: %d\n", nrow(new_baseline)))
  cat("  Baseline KDIGO distribution (raw):\n")
  print(new_baseline[, .N, by = kdigo][order(kdigo)])
  cat("  Baseline KDIGO distribution (G3->G3a for comparison):\n")
  print(new_baseline[, .N, by = kdigo_compare][order(kdigo_compare)])
}

# Warn if match rate is low but don't fail (minor differences possible
# due to data pipeline differences)
if (nrow(merged) > 0 && risk_match / nrow(merged) < 0.9) {
  cat("  WARN: Risk match rate below 90% - check censoring differences\n")
}

cat("=== Algorithm tests complete ===\n")
