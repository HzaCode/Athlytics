library(covr)
library(testthat)

# Ensure we're in the right directory
message("Current working directory: ", getwd())

# Check if files exist before running coverage
source_files <- c(
  "R/calculate_ef.R",
  "R/calculate_acwr.R",
  "R/calculate_decoupling.R",
  "R/calculate_exposure.R",
  "R/calculate_pbs.R",
  "R/plot_acwr.R",
  "R/plot_decoupling.R",
  "R/plot_ef.R",
  "R/plot_exposure.R",
  "R/plot_pbs.R",
  "R/utils.R"
)

test_files <- c(
  "tests/testthat/test-ef.R",
  "tests/testthat/test-acwr.R",
  "tests/testthat/test-calculate_decoupling.R",
  "tests/testthat/test-calculate_exposure.R",
  "tests/testthat/test-decoupling.R",
  "tests/testthat/test-exposure.R",
  "tests/testthat/test-pbs.R",
  "tests/testthat/test-utils.R"
)

# Check if files exist
missing_source <- source_files[!file.exists(source_files)]
missing_test <- test_files[!file.exists(test_files)]

if (length(missing_source) > 0) {
  message("Missing source files: ", paste(missing_source, collapse = ", "))
}

if (length(missing_test) > 0) {
  message("Missing test files: ", paste(missing_test, collapse = ", "))
}

# Use package coverage if file-specific coverage fails
tryCatch({
  cov <- covr::file_coverage(
    source_files = source_files[file.exists(source_files)],
    test_files = test_files[file.exists(test_files)]
  )
}, error = function(e) {
  message("File coverage failed, using package coverage: ", e$message)
  cov <- covr::package_coverage()
})


print(cov)

# Upload coverage with error handling
if (!is.null(cov) && length(cov) > 0) {
  message("Uploading coverage data to Codecov...")
  tryCatch({
    covr::codecov(coverage = cov)
    message("Coverage upload successful!")
  }, error = function(e) {
    message("Coverage upload failed: ", e$message)
    message("Coverage data available but upload failed - this may be due to network issues or Codecov configuration.")
  })
} else {
  message("Coverage object is NULL or empty, skipping upload.")
  message("This may indicate issues with test execution or source file detection.")
} 
