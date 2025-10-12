# Run coverage test with fresh session
message("Current working directory: ", getwd())

# Load covr library
library(covr)

# Use package coverage (more reliable than file coverage)
message("Calculating package coverage...")
cov <- tryCatch({
  package_coverage(
    type = "all",
    quiet = FALSE,
    clean = FALSE
  )
}, error = function(e) {
  message("Error calculating coverage: ", e$message)
  NULL
})

# Print coverage results
if (!is.null(cov) && length(cov) > 0) {
  message("\n=== Coverage Summary ===")
  print(cov)
  
  message("\n=== Coverage Percentage ===")
  print(percent_coverage(cov))
  
  # Upload coverage with error handling
  message("\nUploading coverage data to Codecov...")
  tryCatch({
    codecov(coverage = cov, quiet = FALSE)
    message("Coverage upload successful!")
  }, error = function(e) {
    message("Coverage upload failed: ", e$message)
    message("Coverage data available but upload failed - this may be due to network issues or Codecov configuration.")
  })
} else {
  message("Coverage object is NULL or empty.")
  message("This may indicate issues with test execution or package structure.")
  quit(status = 1)
} 
