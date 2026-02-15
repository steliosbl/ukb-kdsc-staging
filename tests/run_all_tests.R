# Run all validation tests
# Execute from the ukb-kdsc-staging directory after R02-R06 notebooks have been knitted
#
# Usage: Rscript tests/run_all_tests.R

cat("Running all validation tests...\n\n")

tryCatch(source("tests/test_cohort.R"), error = function(e) {
  cat(sprintf("FAIL: test_cohort.R - %s\n", e$message))
})
cat("\n")

tryCatch(source("tests/test_biomarkers.R"), error = function(e) {
  cat(sprintf("FAIL: test_biomarkers.R - %s\n", e$message))
})
cat("\n")

tryCatch(source("tests/test_hospital.R"), error = function(e) {
  cat(sprintf("FAIL: test_hospital.R - %s\n", e$message))
})
cat("\n")

tryCatch(source("tests/test_algorithm.R"), error = function(e) {
  cat(sprintf("FAIL: test_algorithm.R - %s\n", e$message))
})
cat("\n")

cat("All tests complete.\n")
