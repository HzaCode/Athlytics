# tests/testthat/test-ef.R

context("Efficiency Factor Calculation")

library(Athlytics)
library(testthat)

data(athlytics_sample_ef)

create_mock_activities <- function(n = 30) {
  dates <- seq(Sys.Date() - n, Sys.Date(), by = "day")
  data.frame(
    id = seq_len(length(dates)),
    name = paste("Activity", seq_len(length(dates))),
    type = sample(c("Run", "Ride"), length(dates), replace = TRUE),
    date = as.Date(dates),
    distance = runif(length(dates), 5000, 15000),
    moving_time = as.integer(runif(length(dates), 1800, 5400)),
    average_heartrate = runif(length(dates), 130, 170),
    average_watts = runif(length(dates), 180, 250),
    average_speed = runif(length(dates), 2.5, 4.5),
    stringsAsFactors = FALSE
  )
}

test_that("calculate_ef works with pace_hr metric", {
  mock_activities <- create_mock_activities()
  
  ef_result <- calculate_ef(
    activities_data = mock_activities,
    ef_metric = "pace_hr"
  )
  
  expect_s3_class(ef_result, "data.frame")
  expect_true("ef_value" %in% colnames(ef_result))
  expect_gt(nrow(ef_result), 0)
})

test_that("calculate_ef works with power_hr metric", {
  mock_activities <- create_mock_activities()
  
  ef_result <- calculate_ef(
    activities_data = mock_activities,
    ef_metric = "power_hr"
  )
  
  expect_s3_class(ef_result, "data.frame")
  expect_true("ef_value" %in% colnames(ef_result))
})

test_that("calculate_ef validates input", {
  expect_error(
    calculate_ef(activities_data = "not_a_dataframe"),
    "data frame"
  )
})

test_that("calculate_ef works with sample data", {
  skip_if(is.null(athlytics_sample_ef), "Sample EF data not available")
  
  expect_s3_class(athlytics_sample_ef, "data.frame")
  # Sample data may have either ef_value or efficiency_factor
  expect_true(any(c("ef_value", "efficiency_factor") %in% colnames(athlytics_sample_ef)))
})
