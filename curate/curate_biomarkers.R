library(data.table)

# Pull down raw data that has been extracted with Table Exporter
system("mkdir -p raw_data", wait = TRUE)
system("dx download 'common/Biomarkers/raw_data/data.csv' -o raw_data/biomarkers.csv", wait = TRUE)

# Load pre-curated information sheets
biomarker_info <- fread("biomarkers/biomarker_information.csv")
sample_info <- fread("biomarkers/sample_information.csv")

# Filter to only kidney-related biomarkers
kidney_biomarkers <- c("creat", "uricreat", "urialb")
biomarker_info <- biomarker_info[var %in% kidney_biomarkers]

# Load in raw data
raw <- fread("raw_data/biomarkers.csv")

# Split out instance (visit) and array index (repeat measure) fields so they
# are rows instead of columns
visit_repeats <- setdiff(unique(gsub("^p[0-9]+_", "", names(raw))), "eid")
raw <- rbindlist(fill = TRUE, use.names = TRUE, lapply(visit_repeats, function(vr) {
  # Find columns matching this visit repeat pair (e.g. ending in _i0)
  this_cols <- names(raw)[grepl(pattern = paste0(vr, "$"), names(raw))]

  # Filter to these columns
  this_raw <- raw[, .SD, .SDcols = c("eid", this_cols)]

  # Drop repeat visit pair label from column name
  setnames(this_raw, this_cols, gsub(paste0("_", vr, "$"), "", this_cols))

  # Add columns for visit index
  this_raw[, visit_index := as.integer(gsub("^i", "", gsub("_.*", "", vr)))]

  # Move to start of data table
  this_raw <- this_raw[, .SD, .SDcols = c("eid", "visit_index", gsub("_.*", "", this_cols))]

  # Drop instance and array index combinations with all missing data
  # eid, visit_index, and array_index always non-missing
  this_raw <- this_raw[apply(this_raw, 1, function(row) {
    sum(!is.na(row)) > 2L
  })]

  # Return
  this_raw
}))

# Extract serum creatinine (blood biomarker)
blood <- raw[, .SD, .SDcols = c(
  "eid", "visit_index",
  biomarker_info[sample_type != "Urine" & !is.na(UKB.Field.ID), paste0("p", UKB.Field.ID)]
)]

if (ncol(blood) > 2) {
  setnames(
    blood,
    biomarker_info[!is.na(UKB.Field.ID) & sample_type != "Urine", paste0("p", UKB.Field.ID)],
    biomarker_info[!is.na(UKB.Field.ID) & sample_type != "Urine", var]
  )

  # Extract blood biomarker missingness reason where missing due to being above or below
  # detection limits (Reportability field, coding: https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=4917)
  miss <- raw[, .SD, .SDcols = c(
    "eid", "visit_index",
    biomarker_info[sample_type != "Urine" & !is.na(Reportability.Field.ID), paste0("p", Reportability.Field.ID)]
  )]

  setnames(
    miss,
    biomarker_info[!is.na(Reportability.Field.ID) & sample_type != "Urine", paste0("p", Reportability.Field.ID)],
    biomarker_info[!is.na(Reportability.Field.ID) & sample_type != "Urine", var]
  )

  # Melt to long to determine lower/upper detection limits
  blood <- melt(blood, id.vars = c("eid", "visit_index"))
  miss <- melt(miss, id.vars = c("eid", "visit_index"), na.rm = TRUE)

  blood[, below_detection := FALSE]
  blood[miss[value %in% c(2, 4)], on = .(eid, visit_index, variable), below_detection := TRUE]

  blood[, above_detection := FALSE]
  blood[miss[value %in% c(3, 5)], on = .(eid, visit_index, variable), above_detection := TRUE]

  # Set values above/below detection to their respective limits (with small offset of 0.0001)
  lims <- blood[!is.na(value), .(min = min(value), max = max(value)), by = variable]
  blood <- rbind(
    blood[!(below_detection) & !(above_detection)],
    blood[(below_detection)][lims, on = .(variable), nomatch = 0, .(
      eid, visit_index, variable,
      value = i.min - 0.0001,
      below_detection, above_detection
    )],
    blood[(above_detection)][lims, on = .(variable), nomatch = 0, .(
      eid, visit_index, variable,
      value = i.max + 0.0001,
      below_detection, above_detection
    )]
  )

  # Drop missing values
  blood <- blood[!is.na(value)]
} else {
  blood <- data.table(
    eid = integer(), visit_index = integer(), variable = character(),
    value = numeric(), below_detection = logical(), above_detection = logical()
  )
}

# Extract urine biomarkers (albumin and creatinine)
urine <- raw[, .SD, .SDcols = c(
  "eid", "visit_index",
  biomarker_info[sample_type == "Urine" & !is.na(UKB.Field.ID), paste0("p", UKB.Field.ID)]
)]

if (ncol(urine) > 2) {
  setnames(
    urine,
    biomarker_info[!is.na(UKB.Field.ID) & sample_type == "Urine", paste0("p", UKB.Field.ID)],
    biomarker_info[!is.na(UKB.Field.ID) & sample_type == "Urine", var]
  )

  # Extract urine biomarker missingness reason where missing due to being above or below
  # detection limits (Reportability field, coding: https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=4917)
  miss <- raw[, .SD, .SDcols = c(
    "eid", "visit_index",
    biomarker_info[sample_type == "Urine" & !is.na(Reportability.Field.ID), paste0("p", Reportability.Field.ID)]
  )]

  setnames(
    miss,
    biomarker_info[!is.na(Reportability.Field.ID) & sample_type == "Urine", paste0("p", Reportability.Field.ID)],
    biomarker_info[!is.na(Reportability.Field.ID) & sample_type == "Urine", var]
  )

  # Melt to long to determine lower/upper detection limits
  urine <- melt(urine, id.vars = c("eid", "visit_index"))
  miss <- melt(miss, id.vars = c("eid", "visit_index"), na.rm = TRUE)
  miss <- miss[value != ""]

  urine[, below_detection := FALSE]
  urine[miss[value %like% "<"], on = .(eid, visit_index, variable), below_detection := TRUE]

  urine[, above_detection := FALSE]
  urine[miss[value %like% ">"], on = .(eid, visit_index, variable), above_detection := TRUE]

  # Set values above/below detection to their respective limits (with small offset of 0.0001)
  # Unlike blood biomarkers, limits are hard coded into the reportability fields
  lims <- unique(miss[, .(variable, value)])
  lower <- lims[value %like% "<", .(variable, min = as.numeric(gsub("<", "", value)))]
  upper <- lims[value %like% ">", .(variable, max = as.numeric(gsub(">", "", value)))]
  lims <- merge(lower, upper, by = "variable", all = TRUE)

  urine <- rbind(
    urine[!(below_detection) & !(above_detection)],
    urine[(below_detection)][lims, on = .(variable), nomatch = 0, .(
      eid, visit_index, variable,
      value = i.min - 0.0001,
      below_detection, above_detection
    )],
    urine[(above_detection)][lims, on = .(variable), nomatch = 0, .(
      eid, visit_index, variable,
      value = i.max + 0.0001,
      below_detection, above_detection
    )]
  )

  # Drop missing
  urine <- urine[!is.na(value)]
} else {
  urine <- data.table(
    eid = integer(), visit_index = integer(), variable = character(),
    value = numeric(), below_detection = logical(), above_detection = logical()
  )
}

# Build combined biomarker table (only kidney biomarkers)
biomarkers <- rbind(blood, urine)

# Build and save table of information about measurements that were either above
# or below detection limits
limits <- biomarkers[
  (below_detection) | (above_detection),
  .(eid, visit_index, variable, type = ifelse(below_detection, "below detection limit", "above detection limit"))
]
fwrite(limits, file = "biomarkers/measurements_outside_detection.csv")

# Cast biomarker table back to wide format
biomarkers <- dcast(biomarkers, eid + visit_index ~ variable, value.var = "value")
biomarkers <- biomarkers[raw[, .(eid, visit_index)], on = .(eid, visit_index), nomatch = 0]

# Add in QC columns flagging whether missing values are due to samples that were
# not measured (as opposed to missing data for some other reason)
not_measured <- raw[, .(eid, visit_index,
  no_blood_sample = ifelse(!is.na(p20050), TRUE, FALSE),
  no_urine_sample = ifelse(!is.na(p20072), TRUE, FALSE)
)]
not_measured <- not_measured[(no_blood_sample) | (no_urine_sample)]
biomarkers <- merge(not_measured, biomarkers, all = TRUE, by = c("eid", "visit_index"))
biomarkers[is.na(no_blood_sample), no_blood_sample := FALSE]
biomarkers[is.na(no_urine_sample), no_urine_sample := FALSE]

# Add fasting time
biomarkers[raw, on = .(eid, visit_index), fasting_time := p74]

# Reorganise columns - keep only kidney-related biomarkers
kidney_cols <- intersect(c(
  "eid", "visit_index", "no_blood_sample", "no_urine_sample",
  "fasting_time", kidney_biomarkers
), names(biomarkers))
biomarkers <- biomarkers[, .SD, .SDcols = kidney_cols]

# Update biomarker and sample information to only include kidney biomarkers
fwrite(sample_info[var %in% c("eid", "visit_index", "no_blood_sample", "no_urine_sample", "fasting_time")],
  file = "biomarkers/sample_information.csv"
)
fwrite(biomarker_info, file = "biomarkers/biomarker_information.csv")

# Write out
fwrite(biomarkers, quote = FALSE, file = "biomarkers/biomarkers.csv")

# Upload to persistent storage
system("dx upload biomarkers/biomarkers.csv biomarkers/measurements_outside_detection.csv --destination 'common/Biomarkers/'", wait = TRUE)
system("dx upload biomarkers/sample_information.csv biomarkers/biomarker_information.csv --destination 'common/Biomarkers/'", wait = TRUE)

# Send raw data to deletion folder to reduce storage costs
rn <- as.integer(Sys.time())
system(sprintf("dx mv 'common/Biomarkers/raw_data/' 'common/Biomarkers/raw_data_%s'", rn), wait = TRUE)
system(sprintf("dx mv 'common/Biomarkers/raw_data_%s/' trash/", rn), wait = TRUE)
