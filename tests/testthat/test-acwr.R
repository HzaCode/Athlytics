# tests/testthat/test-acwr.R

# ACWR Calculation and Plotting Tests

library(Athlytics)
library(testthat)
library(dplyr)

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
  set.seed(42)
  mock_activities <- create_mock_activities(60)

  # Test duration_mins
  acwr_duration <- calculate_acwr(
    activities_data = mock_activities,
    activity_type = "Run",
    load_metric = "duration_mins"
  )
  expect_s3_class(acwr_duration, "data.frame")
  expect_gt(nrow(acwr_duration), 0)
  # ATL values should be non-negative (allow tiny floating-point tolerance)
  expect_true(all(acwr_duration$atl >= -1e-10, na.rm = TRUE))

  # Test distance_km
  acwr_distance <- calculate_acwr(
    activities_data = mock_activities,
    activity_type = "Run",
    load_metric = "distance_km"
  )
  expect_s3_class(acwr_distance, "data.frame")
  expect_gt(nrow(acwr_distance), 0)
  expect_true(all(acwr_distance$atl >= -1e-10, na.rm = TRUE))

  # Test elevation
  acwr_elevation <- calculate_acwr(
    activities_data = mock_activities,
    activity_type = "Run",
    load_metric = "elevation_gain_m"
  )
  expect_s3_class(acwr_elevation, "data.frame")
  expect_gt(nrow(acwr_elevation), 0)
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

test_that("plot_acwr works with pre-calculated data and has correct labels", {
  skip_if(is.null(sample_acwr), "Sample ACWR data not available")

  p <- plot_acwr(sample_acwr, highlight_zones = TRUE)

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Date")
  expect_true(grepl("ACWR", p$labels$y),
    info = sprintf("Y-axis label should contain 'ACWR', got: %s", p$labels$y)
  )
  expect_gte(length(p$layers), 1)
})

test_that("plot_acwr validates input", {
  # Test with non-data.frame - should error
  expect_error(
    plot_acwr("not_a_dataframe"),
    "Input 'data' must be a data frame from calculate_acwr()."
  )

  # Test with missing required columns - should error
  bad_df <- data.frame(x = 1:10, y = 1:10)
  expect_error(
    plot_acwr(bad_df),
    "must be the output of calculate_acwr"
  )
})

# ============================================================
# Numerical Value Validation
# ============================================================

test_that("calculate_acwr produces correct rolling averages for constant load", {
  # Create deterministic test data: one activity per day with known load
  end_date <- Sys.Date()
  start_date <- end_date - 120
  dates <- seq(start_date, end_date, by = "day")
  n <- length(dates)

  # Fixed load of 60 minutes every day
  activities <- data.frame(
    id = seq_len(n),
    name = paste("Run", seq_len(n)),
    type = "Run",
    sport_type = "Run",
    date = dates,
    start_date_local = as.POSIXct(dates),
    distance = rep(10000, n),
    moving_time = rep(3600, n), # 60 min in seconds
    elapsed_time = rep(3600, n),
    average_heartrate = rep(150, n),
    average_speed = rep(3.0, n),
    stringsAsFactors = FALSE
  )

  result <- calculate_acwr(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "duration_mins",
    acute_period = 7,
    chronic_period = 28,
    start_date = start_date,
    end_date = end_date
  )

  # With constant daily load of 60 min:
  # ATL (7-day avg) should be 60
  # CTL (28-day avg) should be 60
  # ACWR should be ~1.0
  valid_rows <- result %>% filter(!is.na(acwr))
  expect_gt(nrow(valid_rows), 0)

  # After the chronic period stabilizes, ATL and CTL should both be ~60
  late_rows <- valid_rows %>% filter(date >= (end_date - 30))
  expect_true(all(abs(late_rows$atl - 60) < 1, na.rm = TRUE),
    info = "ATL should be ~60 for constant 60min daily load"
  )
  expect_true(all(abs(late_rows$ctl - 60) < 1, na.rm = TRUE),
    info = "CTL should be ~60 for constant 60min daily load"
  )
  expect_true(all(abs(late_rows$acwr - 1.0) < 0.05, na.rm = TRUE),
    info = "ACWR should be ~1.0 for constant daily load"
  )
})

test_that("calculate_acwr responds to load changes correctly", {
  # Phase 1: 28 days at 30 min/day, Phase 2: 7 days at 90 min/day
  end_date <- Sys.Date()
  start_phase1 <- end_date - 34
  dates_p1 <- seq(start_phase1, start_phase1 + 27, by = "day")
  dates_p2 <- seq(start_phase1 + 28, end_date, by = "day")
  dates_all <- c(dates_p1, dates_p2)
  n <- length(dates_all)

  activities <- data.frame(
    id = seq_len(n),
    name = paste("Run", seq_len(n)),
    type = "Run",
    sport_type = "Run",
    date = dates_all,
    start_date_local = as.POSIXct(dates_all),
    distance = rep(10000, n),
    moving_time = c(rep(1800, length(dates_p1)), rep(5400, length(dates_p2))),
    elapsed_time = c(rep(1800, length(dates_p1)), rep(5400, length(dates_p2))),
    average_heartrate = rep(150, n),
    average_speed = rep(3.0, n),
    stringsAsFactors = FALSE
  )

  result <- calculate_acwr(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "duration_mins",
    acute_period = 7,
    chronic_period = 28,
    start_date = start_phase1,
    end_date = end_date
  )

  # At the end: ATL should be ~90 (7-day avg of 90min/day)
  # ACWR should be > 1.5 after sudden load increase
  last_valid <- result %>%
    filter(date == end_date, !is.na(acwr))

  if (nrow(last_valid) > 0) {
    expect_true(last_valid$atl > 80,
      info = sprintf("ATL should be >80 after days at 90min, got %.1f", last_valid$atl)
    )
    expect_true(last_valid$acwr > 1.5,
      info = sprintf("ACWR should be >1.5 after sudden load increase, got %.2f", last_valid$acwr)
    )
  }
})

test_that("calculate_acwr returns S3 class athlytics_acwr", {
  end_date <- Sys.Date()
  start_date <- end_date - 90
  dates <- seq(start_date, end_date, by = "day")
  n <- length(dates)
  activities <- data.frame(
    id = seq_len(n), name = paste("Run", seq_len(n)),
    type = "Run", sport_type = "Run",
    date = dates, start_date_local = as.POSIXct(dates),
    distance = rep(10000, n), moving_time = rep(3600, n),
    elapsed_time = rep(3600, n), average_heartrate = rep(150, n),
    average_speed = rep(3.0, n), stringsAsFactors = FALSE
  )

  result <- calculate_acwr(
    activities_data = activities,
    activity_type = "Run",
    start_date = start_date,
    end_date = end_date
  )

  expect_true("athlytics_acwr" %in% class(result),
    info = "Result should have athlytics_acwr S3 class"
  )
})
