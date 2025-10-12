# tests/testthat/test-ef.R

# Efficiency Factor Calculation Tests

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

test_that("plot_ef works with pre-calculated data", {
  skip_if(is.null(athlytics_sample_ef), "Sample EF data not available")
  
  p <- plot_ef(athlytics_sample_ef)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ef works with activities data", {
  mock_activities <- create_mock_activities(50)
  
  # Test with pace_hr
  p1 <- plot_ef(
    data = mock_activities,
    activity_type = "Run",
    ef_metric = "pace_hr"
  )
  expect_s3_class(p1, "ggplot")
  
  # Test with power_hr
  p2 <- plot_ef(
    data = mock_activities,
    activity_type = "Ride",
    ef_metric = "power_hr"
  )
  expect_s3_class(p2, "ggplot")
})

test_that("plot_ef handles various options", {
  mock_activities <- create_mock_activities(50)
  
  # Test without trend line
  p1 <- plot_ef(
    data = mock_activities,
    activity_type = "Run",
    ef_metric = "pace_hr",
    add_trend_line = FALSE
  )
  expect_s3_class(p1, "ggplot")
  
  # Test with different smoothing method
  p2 <- plot_ef(
    data = mock_activities,
    activity_type = "Run",
    ef_metric = "pace_hr",
    smoothing_method = "lm"
  )
  expect_s3_class(p2, "ggplot")
  
  # Test with date range
  p3 <- plot_ef(
    data = mock_activities,
    activity_type = "Run",
    ef_metric = "pace_hr",
    start_date = Sys.Date() - 30,
    end_date = Sys.Date()
  )
  expect_s3_class(p3, "ggplot")
})

test_that("plot_ef works with ef_df parameter", {
  skip_if(is.null(athlytics_sample_ef), "Sample EF data not available")
  
  # Plot already calculated EF data
  p <- plot_ef(athlytics_sample_ef)
  expect_s3_class(p, "ggplot")
})
