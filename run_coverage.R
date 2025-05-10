library(covr)
library(testthat)


setwd(getwd())


cov <- covr::file_coverage(
  source_files = c(
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
    "R/strava_helpers.R",
    "R/utils.R"
  ),
  test_files = c(
    "tests/testthat/test-ef.R",
    "tests/testthat/test-acwr.R",
    "tests/testthat/test-calculate_decoupling.R",
    "tests/testthat/test-calculate_exposure.R",
    "tests/testthat/test-calculate_pbs.R",
    "tests/testthat/test-decoupling.R",
    "tests/testthat/test-exposure.R",
    "tests/testthat/test-pbs.R",
    "tests/testthat/test-utils.R",
    "tests/testthat/test-strava_helpers.R"
  )
)


print(cov)




if (!is.null(cov)) {
  message("Uploading coverage data to Codecov...")
  covr::codecov(coverage = cov)
} else {
  message("Coverage object is NULL, skipping upload.")
} 
