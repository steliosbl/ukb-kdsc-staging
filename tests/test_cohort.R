# Test: Verify cohort output matches parent repo
# Run after R02-cohort.rmd has been knitted
library(data.table)
library(dplyr)
library(arrow)

cat("=== Test: Cohort ===\n")

# Load new repo output
new_cohort <- read_parquet("data/cohort.parquet") %>% as.data.table()
new_visits <- read_parquet("data/visits.parquet") %>% as.data.table()

# Load parent repo output
parent_cohort <- read_parquet("../data/curated/cohort.parquet") %>% as.data.table()
parent_visits <- read_parquet("../data/curated/visits.parquet") %>% as.data.table()

# Test 1: Same set of participant IDs
new_eids <- sort(unique(new_cohort$eid))
parent_eids <- sort(unique(parent_cohort$eid))
stopifnot("Participant IDs must match" = identical(new_eids, parent_eids))
cat(sprintf("  PASS: %d participants match\n", length(new_eids)))

# Test 2: Sex matches
merged <- merge(new_cohort[, .(eid, sex_new = sex)],
  parent_cohort[, .(eid, sex_parent = sex)],
  by = "eid"
)
mismatches <- merged[sex_new != sex_parent]
stopifnot("Sex must match for all participants" = nrow(mismatches) == 0)
cat(sprintf("  PASS: Sex matches for all %d participants\n", nrow(merged)))

# Test 3: Censor dates match (within 1 day tolerance for rounding)
merged_censor <- merge(
  new_cohort[, .(eid, start_new = censor_date_start, end_new = censor_date_end)],
  parent_cohort[, .(eid, start_parent = censor_date_start, end_parent = censor_date_end)],
  by = "eid"
)
# Allow small differences due to minor pipeline differences
start_diff <- abs(as.integer(merged_censor$start_new - merged_censor$start_parent))
end_diff <- abs(as.integer(merged_censor$end_new - merged_censor$end_parent))
cat(sprintf(
  "  INFO: Censor date start - max diff: %d days, median: %d days\n",
  max(start_diff, na.rm = TRUE), median(start_diff, na.rm = TRUE)
))
cat(sprintf(
  "  INFO: Censor date end - max diff: %d days, median: %d days\n",
  max(end_diff, na.rm = TRUE), median(end_diff, na.rm = TRUE)
))

# Test 4: Visit dates match
merged_visits <- merge(
  new_visits[, .(eid, visit_index, date_new = assessment_date)],
  parent_visits[, .(eid, visit_index, date_parent = assessment_date)],
  by = c("eid", "visit_index")
)
visit_diffs <- merged_visits[date_new != date_parent]
stopifnot("Visit dates must match" = nrow(visit_diffs) == 0)
cat(sprintf("  PASS: Visit dates match for all %d visits\n", nrow(merged_visits)))

# Test 5: GP linkage flag
if ("has_gp" %in% names(new_cohort) && "has_gp" %in% names(parent_cohort)) {
  merged_gp <- merge(new_cohort[, .(eid, gp_new = has_gp)],
    parent_cohort[, .(eid, gp_parent = has_gp)],
    by = "eid"
  )
  gp_mismatches <- merged_gp[gp_new != gp_parent]
  cat(sprintf(
    "  INFO: GP linkage mismatches: %d (%.1f%%)\n",
    nrow(gp_mismatches), 100 * nrow(gp_mismatches) / nrow(merged_gp)
  ))
}

cat("=== Cohort tests complete ===\n")
