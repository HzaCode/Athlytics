# tests/testthat/test-calculate_exposure.R

context("Exposure Calculation")

library(Athlytics)
library(testthat)

# Load sample data
data(athlytics_sample_exposure)

# Create mock activities
create_mock_activities <- function(n = 30) {
  dates <- seq(Sys.Date() - n, Sys.Date(), by = "day")
  data.frame(
    id = seq_len(length(dates)),
    name = paste("Activity", seq_len(length(dates))),
    type = sample(c("Run", "Ride"), length(dates), replace = TRUE),
    date = as.Date(dates),
    distance = runif(length(dates), 1000, 15000),
    moving_time = as.integer(runif(length(dates), 1200, 5400)),
    elapsed_time = as.integer(runif(length(dates), 1200, 5400)),
    average_heartrate = runif(length(dates), 120, 170),
    average_watts = runif(length(dates), 150, 250),
    elevation_gain = runif(length(dates), 0, 500),
    stringsAsFactors = FALSE
  )
}

test_that("calculate_exposure works with local activities data", {
  mock_activities <- create_mock_activities(60)
  
  exposure_result <- calculate_exposure(
    activities_data = mock_activities,
    load_metric = "duration_mins",
    acute_period = 7,
    chronic_period = 28
  )
  
  expect_s3_class(exposure_result, "data.frame")
  expect_true(all(c("date", "atl", "ctl") %in% colnames(exposure_result)))
  expect_gt(nrow(exposure_result), 0)
})

test_that("calculate_exposure validates input", {
  expect_error(
    calculate_exposure(activities_data = "not_a_dataframe"),
    "data frame"
  )
})

test_that("calculate_exposure works with sample data", {
  skip_if(is.null(athlytics_sample_exposure), "Sample exposure data not available")
  
  expect_s3_class(athlytics_sample_exposure, "data.frame")
  expect_true("atl" %in% colnames(athlytics_sample_exposure))
})
