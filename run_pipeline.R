#!/usr/bin/env Rscript
# Master pipeline runner for ukb-kdsc-staging
# Runs all stages in order and validates outputs with tests
# Stops execution if any test fails
#
# Usage: Rscript run_pipeline.R

library(knitr)
print("Starting UK Biobank KDSC Staging Pipeline...")

run_stage <- function(rmd_file, stage_name) {
  cat(sprintf("\n=== Running Stage: %s ===\n", stage_name))
  tryCatch(
    {
      output_file <- tempfile(fileext = ".md")
      knitr::knit(rmd_file, output = output_file, quiet = TRUE)
      cat(sprintf("  COMPLETE: %s\n", stage_name))
      return(TRUE)
    },
    error = function(e) {
      cat(sprintf("  ERROR in %s: %s\n", stage_name, e$message))
      stop(sprintf("Pipeline failed at stage: %s", stage_name))
    }
  )
}

run_test <- function(test_file, test_name) {
  cat(sprintf("\n=== Running Test: %s ===\n", test_name))
  tryCatch(
    {
      source(test_file, local = new.env())
      cat(sprintf("  COMPLETE: %s\n", test_name))
      return(TRUE)
    },
    error = function(e) {
      cat(sprintf("  FAILED: %s - %s\n", test_name, e$message))
      stop(sprintf("Test failed: %s", test_name))
    }
  )
}

cat("========================================\n")
cat("UK Biobank KDSC Staging Pipeline\n")
cat("========================================\n")

# Stage 1: Cohort Definition
run_stage("R02-cohort.rmd", "R02-cohort (Cohort Definition)")
run_test("tests/test_cohort.R", "Cohort Validation")

# Stage 2: Fields Extraction
run_stage("R03-fields.rmd", "R03-fields (Fields Extraction)")

# Stage 3: GP Records Processing
run_stage("R04-gp.rmd", "R04-gp (GP Records)")
run_test("tests/test_biomarkers.R", "Biomarkers Validation")

# Stage 4: Hospital Records Processing
run_stage("R05-hospital.rmd", "R05-hospital (Hospital Records)")
run_test("tests/test_hospital.R", "Hospital Validation")

# Stage 5: KDSC Algorithm
run_stage("R06-algorithm.rmd", "R06-algorithm (KDSC Algorithm)")
run_test("tests/test_algorithm.R", "Algorithm Validation")

# Stage 6: Plotting
run_stage("R07-plot.rmd", "R07-plot (Visualization)")

cat("\n========================================\n")
cat("Pipeline completed successfully!\n")
cat("========================================\n")
