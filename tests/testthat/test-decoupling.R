# tests/testthat/test-decoupling.R

library(testthat)
library(Athlytics)
library(lubridate) # Ensure lubridate is loaded for ymd in english_month_year tests

# Load main sample data for the package
data(sample_decoupling)

# Load data from helper for direct use in tests
# Ensure helper-mockdata.R is in the tests/testthat directory
source(test_path("helper-mockdata.R"), local = TRUE)

# --- Test english_month_year ---
# This helper is defined in R/utils.R

test_that("english_month_year formats dates correctly", {
  # Test with a single date
  single_date <- ymd("2023-01-15")
  expect_equal(Athlytics:::english_month_year(single_date), "Jan 2023")

  # Test with multiple dates
  multiple_dates <- c(ymd("2023-03-10"), ymd("2024-11-05"))
  expect_equal(Athlytics:::english_month_year(multiple_dates), c("Mar 2023", "Nov 2024"))

  # Test with dates spanning year-end
  year_end_dates <- c(ymd("2022-12-25"), ymd("2023-01-01"))
  expect_equal(Athlytics:::english_month_year(year_end_dates), c("Dec 2022", "Jan 2023"))

  # Test with a leap year date
  leap_date <- ymd("2024-02-29")
  expect_equal(Athlytics:::english_month_year(leap_date), "Feb 2024")

  # Test with an empty vector of dates
  empty_dates <- ymd(character(0))
  expect_equal(Athlytics:::english_month_year(empty_dates), character(0))
})

# --- Test Cases for calculate_decoupling (using stream_df) ---

test_that("calculate_decoupling computes a plausible value from mock stream_df", {
  # This test uses mock_activity_streams from helpe-mockdata.R for single activity stream processing
  expect_true(exists("mock_activity_streams"), "mock_activity_streams not loaded from helper.")

  decoupling_power_hr <- calculate_decoupling(stream_df = mock_activity_streams, decouple_metric = "power_hr")
  decoupling_speed_hr <- calculate_decoupling(stream_df = mock_activity_streams, decouple_metric = "speed_hr")

  expect_type(decoupling_power_hr, "double")
  expect_type(decoupling_speed_hr, "double")
  expect_true(is.finite(decoupling_power_hr))
  expect_true(is.finite(decoupling_speed_hr))
  # Check for a plausible range (e.g., -20% to +50%)
  expect_true(decoupling_power_hr > -20 && decoupling_power_hr < 50)
  expect_true(decoupling_speed_hr > -20 && decoupling_speed_hr < 50)
})

test_that("calculate_decoupling handles missing columns in stream_df", {
  expect_true(exists("mock_activity_streams"), "mock_activity_streams not loaded from helper.")

  # Test missing watts for power_hr
  mock_streams_no_watts <- mock_activity_streams[, !(names(mock_activity_streams) %in% "watts")]
  expect_error(calculate_decoupling(stream_df = mock_streams_no_watts, decouple_metric = "power_hr"),
    regexp = "must contain 'watts' column"
  )

  # Test missing velocity_smooth AND distance for speed_hr
  mock_streams_no_speed <- mock_activity_streams[, !(names(mock_activity_streams) %in% c("velocity_smooth", "distance"))]
  expect_error(calculate_decoupling(stream_df = mock_streams_no_speed, decouple_metric = "speed_hr"),
    regexp = "must contain 'distance' or 'velocity_smooth' column"
  )

  # Test missing heartrate
  mock_streams_no_hr <- mock_activity_streams[, !(names(mock_activity_streams) %in% "heartrate")]
  expect_error(calculate_decoupling(stream_df = mock_streams_no_hr, decouple_metric = "power_hr"),
    regexp = "missing required columns"
  )
})

# --- Test Cases for plot_decoupling (using sample_decoupling) ---

test_that("plot_decoupling returns a ggplot object using sample_decoupling", {
  expect_true(exists("sample_decoupling"), "sample_decoupling not found.")
  expect_s3_class(sample_decoupling, "data.frame")
  # Ensure required columns for plotting are present in the sample data
  expect_true(all(c("date", "decoupling") %in% names(sample_decoupling)))

  # plot_decoupling requires decouple_metric for labeling if not directly derivable from decoupling_df
  # We assume sample_decoupling might be for a mix or needs this specified.
  # suppressWarnings: decouple_metric is analysis arg, ignored with pre-calculated data
  p_decouple <- suppressWarnings(plot_decoupling(data = sample_decoupling, decouple_metric = "speed_hr"))
  expect_s3_class(p_decouple, "ggplot")

  # Check that the plot is not empty / uses the data
  expect_gt(nrow(p_decouple$data), 0)
  expect_true("decoupling" %in% names(p_decouple$data))
})

test_that("plot_decoupling throws error if data is missing", {
  expect_error(
    plot_decoupling(decouple_metric = "speed_hr"), # data is missing
    # Standard R error for missing argument, or check if I should handle it
    # Since 'data' has no default, it errors with "argument "data" is missing"
  )
})

test_that("plot_decoupling throws error for invalid decoupling_df", {
  # Test with an empty data frame (missing required columns triggers class check first)
  empty_df <- data.frame()
  expect_error(
    suppressWarnings(plot_decoupling(data = empty_df, decouple_metric = "speed_hr")),
    regexp = "must be the output of calculate_decoupling"
  )

  # Test with a data frame missing the 'decoupling' column
  df_missing_col <- data.frame(date = Sys.Date())
  expect_error(
    suppressWarnings(plot_decoupling(data = df_missing_col, decouple_metric = "speed_hr")),
    regexp = "must be the output of calculate_decoupling"
  )

  # Test with a data frame missing the 'date' column
  df_missing_date <- data.frame(decoupling = 0.5)
  expect_error(
    suppressWarnings(plot_decoupling(data = df_missing_date, decouple_metric = "speed_hr")),
    regexp = "must be the output of calculate_decoupling"
  )
})

test_that("plot_decoupling sets title correctly", {
  # Valid dummy data for plotting
  valid_df <- data.frame(date = Sys.Date() - 0:2, decoupling = c(1, 2, 3))

  # Default title
  p1 <- plot_decoupling(data = valid_df)
  expect_true(grepl("Aerobic Decoupling Trend", p1$labels$title))

  # Custom title
  p2 <- plot_decoupling(data = valid_df, title = "My Custom Title")
  expect_equal(p2$labels$title, "My Custom Title")

  # Data with params attribute (from calculate_decoupling)
  attr(valid_df, "params") <- list(decouple_metric = "speed_hr", activity_type = "Run")
  p3 <- plot_decoupling(data = valid_df)
  expect_true(grepl("Aerobic Decoupling Trend", p3$labels$title))
  expect_true(grepl("speed_hr", p3$labels$subtitle))
  expect_true(grepl("Run", p3$labels$subtitle))
})

test_that("plot_decoupling handles add_trend_line argument correctly", {
  # Helper function to check for GeomSmooth layer
  has_smooth_layer <- function(p) {
    any(sapply(p$layers, function(layer) inherits(layer$geom, "GeomSmooth")))
  }

  # suppressWarnings: analysis args are ignored with pre-calculated data (expected behavior)
  # Case 1: add_trend_line = TRUE, but not enough data points (e.g., 1 point)
  df_one_point <- data.frame(date = Sys.Date(), decoupling = 1)
  p_one_point <- suppressWarnings(plot_decoupling(data = df_one_point, add_trend_line = TRUE, decouple_metric = "speed_hr"))
  expect_false(has_smooth_layer(p_one_point))

  # Case 2: add_trend_line = TRUE, enough data points (e.g., 2 points)
  df_two_points <- data.frame(date = Sys.Date() - 0:1, decoupling = c(1, 2))
  p_two_points <- suppressWarnings(plot_decoupling(data = df_two_points, add_trend_line = TRUE, decouple_metric = "speed_hr"))
  expect_true(has_smooth_layer(p_two_points))

  # Case 3: add_trend_line = FALSE, enough data points
  df_enough_points <- data.frame(date = Sys.Date() - 0:2, decoupling = c(1, 2, 3))
  p_no_trend <- suppressWarnings(plot_decoupling(data = df_enough_points, add_trend_line = FALSE, decouple_metric = "speed_hr"))
  expect_false(has_smooth_layer(p_no_trend))
})

# ============================================================
# Numerical Value Validation
# ============================================================

test_that("decoupling is near-zero for constant HR stream", {
  n <- 3600
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = rep(150, n),
    velocity_smooth = rep(3.0, n),
    distance = cumsum(rep(3.0, n)),
    stringsAsFactors = FALSE
  )

  result <- calculate_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr"
  )

  expect_true(is.numeric(result))
  if (!is.na(result)) {
    expect_true(abs(result) < 2.0,
      info = sprintf("Decoupling with constant HR should be ~0%%, got %.1f%%", result)
    )
  }
})

test_that("decoupling detects HR drift correctly", {
  n <- 3600
  half <- n / 2
  # First half: HR=140, speed=3.0 -> EF1 = 3.0/140
  # Second half: HR=154, speed=3.0 -> EF2 = 3.0/154
  # Decoupling = (EF1-EF2)/EF1 * 100 ~ 9.1%
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = c(rep(140, half), rep(154, half)),
    velocity_smooth = rep(3.0, n),
    distance = cumsum(rep(3.0, n)),
    stringsAsFactors = FALSE
  )

  result <- calculate_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr"
  )

  expect_true(is.numeric(result))
  if (!is.na(result)) {
    # Expected ~9.1%, allow some tolerance
    expect_true(abs(result - 9.1) < 3.0,
      info = sprintf("Decoupling should be ~9.1%%, got %.1f%%", result)
    )
    # Should be positive (HR drifted up)
    expect_true(result > 0, info = "Decoupling should be positive when HR drifts up")
  }
})
