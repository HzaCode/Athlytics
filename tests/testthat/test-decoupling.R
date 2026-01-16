# tests/testthat/test-decoupling.R

library(testthat)
library(Athlytics)
library(lubridate) # Ensure lubridate is loaded for ymd in english_month_year tests
# library(rStrava) # No longer needed directly if we use stream_df or sample data

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
  decoupling_pace_hr <- calculate_decoupling(stream_df = mock_activity_streams, decouple_metric = "pace_hr")

  expect_type(decoupling_power_hr, "double")
  expect_type(decoupling_pace_hr, "double")
  expect_true(is.finite(decoupling_power_hr))
  expect_true(is.finite(decoupling_pace_hr))
  # Check for a plausible range (e.g., -20% to +50%)
  expect_true(decoupling_power_hr > -20 && decoupling_power_hr < 50)
  expect_true(decoupling_pace_hr > -20 && decoupling_pace_hr < 50)
})

test_that("calculate_decoupling handles missing columns in stream_df", {
  expect_true(exists("mock_activity_streams"), "mock_activity_streams not loaded from helper.")

  # Test missing watts for power_hr
  mock_streams_no_watts <- mock_activity_streams[, !(names(mock_activity_streams) %in% "watts")]
  expect_error(calculate_decoupling(stream_df = mock_streams_no_watts, decouple_metric = "power_hr"),
    regexp = "must contain 'watts' column"
  )

  # Test missing velocity_smooth AND distance for pace_hr
  mock_streams_no_speed <- mock_activity_streams[, !(names(mock_activity_streams) %in% c("velocity_smooth", "distance"))]
  expect_error(calculate_decoupling(stream_df = mock_streams_no_speed, decouple_metric = "pace_hr"),
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
  p_decouple <- plot_decoupling(decoupling_df = sample_decoupling, decouple_metric = "pace_hr")
  expect_s3_class(p_decouple, "ggplot")

  # Check that the plot is not empty / uses the data
  expect_gt(nrow(p_decouple$data), 0)
  expect_true("decoupling" %in% names(p_decouple$data))
})

test_that("plot_decoupling throws error if data is missing and decoupling_df is NULL", {
  expect_error(
    plot_decoupling(decouple_metric = "pace_hr"), # data is missing by default
    regexp = "Either 'data' or 'decoupling_df' must be provided."
  )
})

test_that("plot_decoupling returns a blank plot with warning for invalid decoupling_df", {
  # Test with an empty data frame
  empty_df <- data.frame()
  expect_warning(
    p_empty <- plot_decoupling(decoupling_df = empty_df, decouple_metric = "pace_hr"),
    regexp = "No valid decoupling data available to plot"
  )
  expect_s3_class(p_empty, "ggplot")
  expect_true(grepl("No decoupling data to plot", p_empty$labels$title, ignore.case = TRUE))

  # Test with a data frame missing the 'decoupling' column
  df_missing_col <- data.frame(date = Sys.Date())
  expect_warning(
    p_missing_col <- plot_decoupling(decoupling_df = df_missing_col, decouple_metric = "pace_hr"),
    regexp = "No valid decoupling data available to plot"
  )
  expect_s3_class(p_missing_col, "ggplot")
  expect_true(grepl("No decoupling data to plot", p_missing_col$labels$title, ignore.case = TRUE))

  # Test with a data frame missing the 'date' column
  df_missing_date <- data.frame(decoupling = 0.5)
  expect_warning(
    p_missing_date <- plot_decoupling(decoupling_df = df_missing_date, decouple_metric = "pace_hr"),
    regexp = "No valid decoupling data available to plot"
  )
  expect_s3_class(p_missing_date, "ggplot")
  expect_true(grepl("No decoupling data to plot", p_missing_date$labels$title, ignore.case = TRUE))
})

test_that("plot_decoupling sets title correctly based on activity_type and data", {
  # Valid dummy data for plotting
  valid_df <- data.frame(date = Sys.Date() - 0:2, decoupling = c(1, 2, 3))

  # Case 1: activity_type is a single string
  p1 <- plot_decoupling(decoupling_df = valid_df, activity_type = "MyRun", decouple_metric = "pace_hr")
  expect_true(grepl("Trend for MyRun", p1$labels$title))

  # Case 2: activity_type (plot arg) is vector, decoupling_df has single activity_type column
  df_single_type_col <- data.frame(
    date = Sys.Date() - 0:2,
    decoupling = c(1, 2, 3),
    activity_type = rep("SpecificRunType", 3)
  )
  p2 <- plot_decoupling(decoupling_df = df_single_type_col, activity_type = c("Run", "Ride"), decouple_metric = "pace_hr")
  expect_true(grepl("Trend for SpecificRunType", p2$labels$title))

  # Case 3: activity_type (plot arg) is vector, decoupling_df has multiple activity_types in its column
  df_multi_type_col <- data.frame(
    date = Sys.Date() - 0:3,
    decoupling = c(1, 2, 3, 4),
    activity_type = c("RunA", "RunA", "RunB", "RunB")
  )
  p3 <- plot_decoupling(decoupling_df = df_multi_type_col, activity_type = c("RunA", "RunB"), decouple_metric = "pace_hr")
  expect_true(grepl("Trend for Selected Activities", p3$labels$title)) # Expect generic

  # Case 4: activity_type (plot arg) is vector, decoupling_df does NOT have activity_type column
  p4 <- plot_decoupling(decoupling_df = valid_df, activity_type = c("Run", "Ride"), decouple_metric = "pace_hr")
  expect_true(grepl("Trend for Selected Activities", p4$labels$title)) # Expect generic

  # Case 5: activity_type (plot arg) is default (vector), decoupling_df does NOT have activity_type column
  p5 <- plot_decoupling(decoupling_df = valid_df, decouple_metric = "pace_hr") # activity_type uses default
  expect_true(grepl("Trend for Selected Activities", p5$labels$title)) # Expect generic
})

test_that("plot_decoupling handles add_trend_line argument correctly", {
  # Helper function to check for GeomSmooth layer
  has_smooth_layer <- function(p) {
    any(sapply(p$layers, function(layer) inherits(layer$geom, "GeomSmooth")))
  }

  # Case 1: add_trend_line = TRUE, but not enough data points (e.g., 1 point)
  df_one_point <- data.frame(date = Sys.Date(), decoupling = 1)
  p_one_point <- plot_decoupling(decoupling_df = df_one_point, add_trend_line = TRUE, decouple_metric = "pace_hr")
  expect_false(has_smooth_layer(p_one_point))

  # Case 2: add_trend_line = TRUE, enough data points (e.g., 2 points)
  df_two_points <- data.frame(date = Sys.Date() - 0:1, decoupling = c(1, 2))
  p_two_points <- plot_decoupling(decoupling_df = df_two_points, add_trend_line = TRUE, decouple_metric = "pace_hr")
  expect_true(has_smooth_layer(p_two_points))

  # Case 3: add_trend_line = FALSE, enough data points
  df_enough_points <- data.frame(date = Sys.Date() - 0:2, decoupling = c(1, 2, 3))
  p_no_trend <- plot_decoupling(decoupling_df = df_enough_points, add_trend_line = FALSE, decouple_metric = "pace_hr")
  expect_false(has_smooth_layer(p_no_trend))
})

test_that("plot_decoupling handles NULL/empty df from internal calculate_decoupling call", {
  skip("API mode is deprecated; test no longer applicable as stoken parameter removed")
})

# --- Tests for API path of calculate_decoupling (kept separate, may fail/skip without real token/mocking) ---
# Dummy stoken for API path tests (basic signature checks)
dummy_stoken <- structure(list(token = list(access_token = "dummy")), class = "Token2.0")

test_that("calculate_decoupling (API path) throws error for deprecated API mode", {
  skip("API mode is deprecated; stoken parameter removed")
})

test_that("calculate_decoupling (API path) rejects non-Token2.0 stoken", {
  skip("API mode is deprecated; stoken parameter removed")
})

# Mock for rStrava::get_activity_streams specific to decoupling tests
# Needs to return a dataframe with time, heartrate, and distance/watts
mock_rStrava_get_activity_streams_decoupling <- function(act_data, acts, stoken, types, resolution) {
  # message(sprintf("MOCK get_activity_streams called for index: %d, types: %s", acts, paste(types, collapse=", "))) # Debug
  # Use the existing mock_activity_streams, assuming it has the needed columns
  # In a real scenario, this might need to vary based on 'acts' or types requested
  if (exists("mock_activity_streams")) {
    # Ensure the mock stream has the requested types
    if (all(types %in% names(mock_activity_streams))) {
      return(mock_activity_streams[, types, drop = FALSE])
    } else {
      warning("Mock get_activity_streams called but mock_activity_streams doesn't have all requested types.")
      return(NULL) # Simulate failure if types mismatch drastically
    }
  } else {
    stop("Mock error: mock_activity_streams not found in test environment.")
  }
}

test_that("calculate_decoupling (API path) works with mocked API calls", {
  # Use mocks defined in test-strava_helpers.R or define similar ones here
  # Ensure mock_activity_list_list is loaded from helper
  expect_true(exists("mock_activity_list_list"), "mock_activity_list_list not loaded from helper.")
  expect_true(exists("mock_activity_streams"), "mock_activity_streams not loaded from helper.") # Needed for stream mock

  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list, # Use the list mock
    get_activity_streams = mock_rStrava_get_activity_streams_decoupling # Use the stream mock
  )

  # API mode is deprecated, so this test is skipped
  skip("API mode is deprecated; decoupling now uses local files only")

  # Check the output structure
  expect_s3_class(result_df, "data.frame")
  expect_true(all(c("date", "decoupling") %in% names(result_df)))
  expect_s3_class(result_df$date, "Date")
  expect_type(result_df$decoupling, "double")

  # Check the number of results (should correspond to the number of 'Run' activities in mock_activity_list_list)
  # Mock has 2 Runs (id 1 and 3)
  expect_equal(nrow(result_df), 2)

  # Check specific decoupling values (optional, depends on stability of mock data calculation)
  # This requires running the decoupling logic manually on mock_activity_streams
  # For now, checking structure and count is a good start.
})

# Test skipped as API call would fail without proper mocking or real token
test_that("calculate_decoupling (API path) structure check (SKIPPED)", {
  skip_on_cran()
  skip("Skipping API call test for calculate_decoupling; requires network or full API mock.")
  # Original test logic commented out as it relies on an actual or complex mocked API call:
  # result_df <- tryCatch({
  #    calculate_decoupling(stoken = dummy_stoken, activity_type = "Run", decouple_metric = "pace_hr", max_activities = 2, min_duration_mins = 1)
  #  }, error = function(e) { NULL })
  # if (is.null(result_df)) {
  #     skip("Test skipped as API call failed or function returned NULL, likely due to missing mock/real token.")
  # }
  # # else { # If somehow it didn't fail, check structure
  # #   expect_s3_class(result_df, "data.frame")
  # # }
})
