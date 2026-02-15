# Test: Verify hospital code matching output matches parent repo
# Run after R05-hospital.rmd has been knitted
library(data.table)
library(dplyr)
library(arrow)

cat("=== Test: Hospital Records & Code Matching ===\n\n")

# Load new repo outputs
new_matched <- read_parquet("data/ckd_matched_events.parquet") %>% as.data.table()
new_sr <- read_parquet("data/selfreport_kidney.parquet") %>% as.data.table()

# Load parent repo outputs
parent_matched <- read_parquet("../ckm-risk-prediction/data/curated/ckd_matched_codes.parquet") %>% as.data.table()
parent_timeline <- read_parquet("../ckm-risk-prediction/data/curated/ckd_timeline.parquet") %>% as.data.table()
parent_sr <- read_parquet("../ckm-risk-prediction/data/curated/medical_history.parquet") %>% as.data.table()

# Test 1: Matched CKD events comparison
new_events <- nrow(new_matched)
new_participants <- uniqueN(new_matched$eid)
parent_events <- nrow(parent_matched)
parent_participants <- uniqueN(parent_matched$eid)
event_diff <- new_events - parent_events
eid_diff <- new_participants - parent_participants
event_pct <- 100 * event_diff / parent_events
eid_pct <- 100 * eid_diff / parent_participants

cat(sprintf("Matched CKD Events:\n"))
cat(sprintf(
  "  Events:       New=%d, Parent=%d, Diff=%+d (%+.2f%%)\n",
  new_events, parent_events, event_diff, event_pct
))
cat(sprintf(
  "  Participants: New=%d, Parent=%d, Diff=%+d (%+.2f%%)\n",
  new_participants, parent_participants, eid_diff, eid_pct
))

stopifnot(
  "Matched event counts should be within 0.5%" = abs(event_pct) < 0.5
)
stopifnot(
  "Matched participant counts should be within 0.5%" = abs(eid_pct) < 0.5
)
cat("  PASS: Event and participant counts within 0.5%\n")

# Test 2: Self-reported kidney events
parent_kidney <- parent_sr[field_id == 20002 & code %in% c(1192, 1193, 1194, 1609)]
new_sr_events <- nrow(new_sr)
parent_sr_events <- nrow(parent_kidney)
new_sr_eids <- uniqueN(new_sr$eid)
parent_sr_eids <- uniqueN(parent_kidney$eid)
sr_event_diff <- new_sr_events - parent_sr_events
sr_eid_diff <- new_sr_eids - parent_sr_eids
sr_event_pct <- 100 * sr_event_diff / parent_sr_events
sr_eid_pct <- 100 * sr_eid_diff / parent_sr_eids

cat(sprintf("\nSelf-Reported Kidney Disease:\n"))
cat(sprintf(
  "  Events:       New=%d, Parent=%d, Diff=%+d (%+.2f%%)\n",
  new_sr_events, parent_sr_events, sr_event_diff, sr_event_pct
))
cat(sprintf(
  "  Participants: New=%d, Parent=%d, Diff=%+d (%+.2f%%)\n",
  new_sr_eids, parent_sr_eids, sr_eid_diff, sr_eid_pct
))

stopifnot(
  "Self-reported kidney participant counts should match within 0.5%" =
    abs(sr_eid_pct) < 0.5
)
cat("  PASS: Participant counts within 0.5%\n")

# Test 3: Source distribution comparison
cat("\nMatched Events by Source:\n")
new_by_source <- new_matched[, .N, by = source][order(source)]
parent_by_source <- parent_matched[, .N, by = source][order(source)]

# Merge for comparison
source_comparison <- merge(
  new_by_source,
  parent_by_source,
  by = "source",
  all = TRUE,
  suffixes = c("_new", "_parent")
)
source_comparison[is.na(N_new), N_new := 0]
source_comparison[is.na(N_parent), N_parent := 0]
source_comparison[, diff := N_new - N_parent]
source_comparison[, pct_diff := 100 * diff / N_parent]

for (i in seq_len(nrow(source_comparison))) {
  cat(sprintf(
    "  %-20s: New=%6d, Parent=%6d, Diff=%+6d (%+.1f%%)\n",
    source_comparison$source[i],
    source_comparison$N_new[i],
    source_comparison$N_parent[i],
    source_comparison$diff[i],
    source_comparison$pct_diff[i]
  ))
}

# Test 4: Event name distribution comparison
cat("\nMatched Events by Name (top 10):\n")
new_by_name <- new_matched[, .N, by = name][order(-N)][1:min(10, .N)]
parent_by_name <- parent_matched[, .N, by = name][order(-N)][1:min(10, .N)]

# Show top names from new
for (i in seq_len(min(10, nrow(new_by_name)))) {
  parent_count <- parent_matched[name == new_by_name$name[i], .N]
  if (parent_count > 0) {
    diff <- new_by_name$N[i] - parent_count
    pct_diff <- 100 * diff / parent_count
    cat(sprintf(
      "  %-30s: New=%6d, Parent=%6d, Diff=%+6d (%+.1f%%)\n",
      new_by_name$name[i], new_by_name$N[i], parent_count, diff, pct_diff
    ))
  } else {
    cat(sprintf(
      "  %-30s: New=%6d, Parent=%6d (NEW)\n",
      new_by_name$name[i], new_by_name$N[i], 0
    ))
  }
}

# Test 5: Compare total unique participants with any CKD evidence
new_total_eids <- uniqueN(c(new_matched$eid, new_sr$eid))
parent_total_eids <- uniqueN(c(parent_matched$eid, parent_kidney$eid))
total_diff <- new_total_eids - parent_total_eids
total_pct <- 100 * total_diff / parent_total_eids

cat(sprintf("\nTotal Unique Participants with CKD Evidence:\n"))
cat(sprintf(
  "  New=%d, Parent=%d, Diff=%+d (%+.2f%%)\n",
  new_total_eids, parent_total_eids, total_diff, total_pct
))

cat("\n=== Hospital & code matching tests complete ===\n")
