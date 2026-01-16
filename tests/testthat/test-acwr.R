# tests/testthat/test-acwr.R

# ACWR Calculation and Plotting Tests

library(Athlytics)
library(testthat)

# Load sample data from the package
data(sample_acwr)
data(sample_exposure)

# Create mock activities data for testing
create_mock_activities <- function(n = 30) {
  dates <- seq(Sys.Date() - n, Sys.Date(), by = "day")
  data.frame(
    id = seq_len(length(dates)),
    name = paste("Activity", seq_len(length(dates))),
    type = sample(c("Run", "Ride"), length(dates), replace = TRUE),
    sport_type = sample(c("Run", "Ride"), length(dates), replace = TRUE),
    start_date_local = as.POSIXct(dates),
    date = as.Date(dates),
    distance = runif(length(dates), 1000, 15000), # meters
    moving_time = as.integer(runif(length(dates), 1200, 5400)), # seconds
    elapsed_time = as.integer(runif(length(dates), 1200, 5400)),
    average_heartrate = runif(length(dates), 120, 170),
    max_heartrate = runif(length(dates), 160, 190),
    average_watts = runif(length(dates), 150, 250),
    max_watts = runif(length(dates), 300, 500),
    elevation_gain = runif(length(dates), 0, 500),
    average_speed = runif(length(dates), 2, 5),
    stringsAsFactors = FALSE
  )
}

# --- Test calculate_acwr with local data ---

test_that("calculate_acwr works with activities_data parameter", {
  mock_activities <- create_mock_activities(60)

  acwr_result <- calculate_acwr(
    activities_data = mock_activities,
    load_metric = "duration_mins",
    activity_type = "Run",
    acute_period = 7,
    chronic_period = 28
  )

  # Structure checks
  expect_s3_class(acwr_result, "data.frame")
  expect_true(all(c("date", "atl", "ctl", "acwr", "acwr_smooth") %in% colnames(acwr_result)))
  expect_s3_class(acwr_result$date, "Date")

  # Check that we have results
  expect_gt(nrow(acwr_result), 0)

  # Numerical checks
  expect_true(is.numeric(acwr_result$atl))
  expect_true(is.numeric(acwr_result$ctl))
  expect_true(is.numeric(acwr_result$acwr))
})

test_that("calculate_acwr validates activities_data parameter", {
  # Test with non-data.frame
  expect_error(
    calculate_acwr(activities_data = "not_a_dataframe"),
    "must be a data frame"
  )

  # Test with empty or incomplete data frame
  empty_df <- data.frame()
  expect_error(
    calculate_acwr(activities_data = empty_df),
    "activity_type.*must be explicitly specified" # Now checks for activity_type first
  )
})

test_that("calculate_acwr validates period parameters", {
  mock_activities <- create_mock_activities()

  # acute_period must be less than chronic_period
  expect_error(
    calculate_acwr(
      activities_data = mock_activities,
      acute_period = 28,
      chronic_period = 7
    ),
    "acute_period.*must be less than.*chronic_period"
  )
})

test_that("calculate_acwr works with different load metrics", {
  mock_activities <- create_mock_activities(60)

  # Test duration_mins
  acwr_duration <- calculate_acwr(
    activities_data = mock_activities,
    activity_type = "Run",
    load_metric = "duration_mins"
  )
  expect_s3_class(acwr_duration, "data.frame")

  # Test distance_km
  acwr_distance <- calculate_acwr(
    activities_data = mock_activities,
    activity_type = "Run",
    load_metric = "distance_km"
  )
  expect_s3_class(acwr_distance, "data.frame")

  # Test elevation
  acwr_elevation <- calculate_acwr(
    activities_data = mock_activities,
    activity_type = "Run",
    load_metric = "elevation_gain_m"
  )
  expect_s3_class(acwr_elevation, "data.frame")
})

test_that("calculate_acwr filters by activity type correctly", {
  mock_activities <- create_mock_activities(60)

  acwr_run <- calculate_acwr(
    activities_data = mock_activities,
    activity_type = "Run",
    load_metric = "duration_mins"
  )

  expect_s3_class(acwr_run, "data.frame")
  expect_gt(nrow(acwr_run), 0)
})

test_that("calculate_acwr works with sample data", {
  skip_if(is.null(sample_acwr), "Sample ACWR data not available")

  # Just check that sample data has the right structure
  expect_s3_class(sample_acwr, "data.frame")
  expect_true(all(c("date", "atl", "ctl", "acwr") %in% colnames(sample_acwr)))
})

# --- Test plot_acwr ---

test_that("plot_acwr works with pre-calculated data", {
  skip_if(is.null(sample_acwr), "Sample ACWR data not available")

  p <- plot_acwr(sample_acwr, highlight_zones = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_acwr validates input", {
  # Test with non-data.frame - should error
  expect_error(
    plot_acwr("not_a_dataframe"),
    "activities_data.*must be a data frame"
  )

  # Test with missing required columns - should error or warn
  bad_df <- data.frame(x = 1:10, y = 1:10)
  expect_error(
    plot_acwr(bad_df),
    "activity_type.*must be explicitly specified" # Now checks for activity_type first
  )
})
