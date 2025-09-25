# tests/testthat/test-decoupling.R

library(testthat)
library(Athlytics)
library(lubridate)
# library(rStrava) # No longer needed directly if we use stream_df or athlytics_sample_data

# Load main sample data for the package
data(athlytics_sample_data)

# Load data from helper for direct use in tests
  # Check helper-mockdata.R is in tests/testthat directory
source(test_path("helper-mockdata.R"), local = TRUE)

# --- Test explicit_english_month_year --- 
# This helper is defined in R/plot_decoupling.R

test_that("explicit_english_month_year formats dates correctly", {
  # Need to source the file where explicit_english_month_year is defined, 
  # or make it available to the test environment if it's not exported.
  # Assuming it's accessible after `library(Athlytics)` if R/plot_decoupling.R is part of the package build.
  # Assuming function is available via package loading

  # Test with a single date
  single_date <- ymd("2023-01-15")
  expect_equal(athlytics:::explicit_english_month_year(single_date), "Jan 2023")

  # Test with multiple dates
  multiple_dates <- c(ymd("2023-03-10"), ymd("2024-11-05"))
  expect_equal(athlytics:::explicit_english_month_year(multiple_dates), c("Mar 2023", "Nov 2024"))

  # Test with dates spanning year-end
  year_end_dates <- c(ymd("2022-12-25"), ymd("2023-01-01"))
  expect_equal(athlytics:::explicit_english_month_year(year_end_dates), c("Dec 2022", "Jan 2023"))

  # Test with a leap year date
  leap_date <- ymd("2024-02-29")
  expect_equal(athlytics:::explicit_english_month_year(leap_date), "Feb 2024")
  
  # Test with an empty vector of dates
  empty_dates <- ymd(character(0))
  expect_equal(athlytics:::explicit_english_month_year(empty_dates), character(0))
  
  # Test with NA date - depends on how lubridate::month/year handle NA
  # lubridate::month(NA) is NA_integer_, lubridate::year(NA) is NA_integer_
  # eng_months[NA_integer_] is NA_character_, paste(NA_character_, NA_integer_) is NA_character_
  na_date <- ymd(NA)
  expect_equal(athlytics:::explicit_english_month_year(na_date), NA_character_)
})

# --- Test Cases for calculate_decoupling (using stream_df) ---

test_that("calculate_decoupling computes a plausible value from mock stream_df", {
  # This test uses mock_activity_streams from helpe-mockdata.R for single activity stream processing
  expect_true(exists("mock_activity_streams"), "mock_activity_streams not loaded from helper.")
  
  decoupling_power_hr <- calculate_decoupling(stream_df = mock_activity_streams, decouple_metric = "Power_HR")
  decoupling_pace_hr <- calculate_decoupling(stream_df = mock_activity_streams, decouple_metric = "Pace_HR")
  
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

  # Test missing watts for Power_HR
  mock_streams_no_watts <- mock_activity_streams[, !(names(mock_activity_streams) %in% "watts")]
  expect_error(calculate_decoupling(stream_df = mock_streams_no_watts, decouple_metric = "Power_HR"), 
                 regexp = "Provided `stream_df` is invalid or missing required columns")
                 
  # Test missing velocity_smooth AND distance for Pace_HR
  mock_streams_no_speed <- mock_activity_streams[, !(names(mock_activity_streams) %in% c("velocity_smooth", "distance"))]
  expect_error(calculate_decoupling(stream_df = mock_streams_no_speed, decouple_metric = "Pace_HR"), 
                 regexp = "Provided `stream_df` is invalid or missing required columns")
                 
  # Test missing heartrate
  mock_streams_no_hr <- mock_activity_streams[, !(names(mock_activity_streams) %in% "heartrate")]
  expect_error(calculate_decoupling(stream_df = mock_streams_no_hr, decouple_metric = "Power_HR"),
                 regexp = "Provided `stream_df` is invalid or missing required columns")
})

# --- Test Cases for plot_decoupling (using athlytics_sample_data) ---

test_that("plot_decoupling returns a ggplot object using athlytics_sample_data", {
  expect_true(exists("athlytics_sample_decoupling"), "athlytics_sample_decoupling not found in athlytics_sample_data.")
  expect_s3_class(athlytics_sample_decoupling, "data.frame")
  # Check required columns for plotting are present
  expect_true(all(c("date", "decoupling") %in% names(athlytics_sample_decoupling)))
  
  # plot_decoupling requires decouple_metric for labeling if not directly derivable from decoupling_df
  # We assume athlytics_sample_decoupling might be for a mix or needs this specified.
  p_decouple <- plot_decoupling(decoupling_df = athlytics_sample_decoupling, decouple_metric = "Pace_HR")
  expect_s3_class(p_decouple, "ggplot")
  
  # Check that the plot is not empty / uses the data
  expect_gt(nrow(p_decouple$data), 0)
  expect_true("decoupling" %in% names(p_decouple$data))
})

test_that("plot_decoupling throws error if stoken is missing and decoupling_df is NULL", {
  expect_error(
    plot_decoupling(decouple_metric = "Pace_HR"), # stoken is missing by default
    regexp = "Either 'stoken' or 'decoupling_df' must be provided."
  )
})

test_that("plot_decoupling returns a blank plot with warning for invalid decoupling_df", {
  # Test with an empty data frame
  empty_df <- data.frame()
  expect_warning(
    p_empty <- plot_decoupling(decoupling_df = empty_df, decouple_metric = "Pace_HR"),
    regexp = "No valid decoupling data available to plot"
  )
  expect_s3_class(p_empty, "ggplot")
  expect_true(grepl("No decoupling data to plot", p_empty$labels$title, ignore.case = TRUE))

  # Test with a data frame missing the 'decoupling' column
  df_missing_col <- data.frame(date = Sys.Date())
  expect_warning(
    p_missing_col <- plot_decoupling(decoupling_df = df_missing_col, decouple_metric = "Pace_HR"),
    regexp = "No valid decoupling data available to plot"
  )
  expect_s3_class(p_missing_col, "ggplot")
  expect_true(grepl("No decoupling data to plot", p_missing_col$labels$title, ignore.case = TRUE))
  
  # Test with a data frame missing the 'date' column
  df_missing_date <- data.frame(decoupling = 0.5)
  expect_warning(
    p_missing_date <- plot_decoupling(decoupling_df = df_missing_date, decouple_metric = "Pace_HR"),
    regexp = "No valid decoupling data available to plot"
  )
  expect_s3_class(p_missing_date, "ggplot")
  expect_true(grepl("No decoupling data to plot", p_missing_date$labels$title, ignore.case = TRUE))
})

test_that("plot_decoupling sets title correctly based on activity_type and data", {
  # Valid dummy data for plotting
  valid_df <- data.frame(date = Sys.Date() - 0:2, decoupling = c(1, 2, 3))
  
  # Case 1: activity_type is a single string
  p1 <- plot_decoupling(decoupling_df = valid_df, activity_type = "MyRun", decouple_metric = "Pace_HR")
  expect_true(grepl("Trend for MyRun", p1$labels$title))

  # Case 2: activity_type (plot arg) is vector, decoupling_df has single activity_type column
  df_single_type_col <- data.frame(
    date = Sys.Date() - 0:2, 
    decoupling = c(1, 2, 3), 
    activity_type = rep("SpecificRunType", 3)
  )
  p2 <- plot_decoupling(decoupling_df = df_single_type_col, activity_type = c("Run", "Ride"), decouple_metric = "Pace_HR")
  expect_true(grepl("Trend for SpecificRunType", p2$labels$title))

  # Case 3: activity_type (plot arg) is vector, decoupling_df has multiple activity_types in its column
  df_multi_type_col <- data.frame(
    date = Sys.Date() - 0:3, 
    decoupling = c(1, 2, 3, 4), 
    activity_type = c("RunA", "RunA", "RunB", "RunB")
  )
  p3 <- plot_decoupling(decoupling_df = df_multi_type_col, activity_type = c("RunA", "RunB"), decouple_metric = "Pace_HR")
  expect_true(grepl("Trend for Selected Activities", p3$labels$title)) # Expect generic
  
  # Case 4: activity_type (plot arg) is vector, decoupling_df does NOT have activity_type column
  p4 <- plot_decoupling(decoupling_df = valid_df, activity_type = c("Run", "Ride"), decouple_metric = "Pace_HR")
  expect_true(grepl("Trend for Selected Activities", p4$labels$title)) # Expect generic
  
  # Case 5: activity_type (plot arg) is default (vector), decoupling_df does NOT have activity_type column
  p5 <- plot_decoupling(decoupling_df = valid_df, decouple_metric = "Pace_HR") # activity_type uses default
  expect_true(grepl("Trend for Selected Activities", p5$labels$title)) # Expect generic
})

test_that("plot_decoupling handles add_trend_line argument correctly", {
  # Helper function to check for GeomSmooth layer
  has_smooth_layer <- function(p) {
    any(sapply(p$layers, function(layer) inherits(layer$geom, "GeomSmooth")))
  }

  # Case 1: add_trend_line = TRUE, but not enough data points (e.g., 1 point)
  df_one_point <- data.frame(date = Sys.Date(), decoupling = 1)
  p_one_point <- plot_decoupling(decoupling_df = df_one_point, add_trend_line = TRUE, decouple_metric = "Pace_HR")
  expect_false(has_smooth_layer(p_one_point))

  # Case 2: add_trend_line = TRUE, enough data points (e.g., 2 points)
  df_two_points <- data.frame(date = Sys.Date() - 0:1, decoupling = c(1, 2))
  p_two_points <- plot_decoupling(decoupling_df = df_two_points, add_trend_line = TRUE, decouple_metric = "Pace_HR")
  expect_true(has_smooth_layer(p_two_points))
  
  # Case 3: add_trend_line = FALSE, enough data points
  df_enough_points <- data.frame(date = Sys.Date() - 0:2, decoupling = c(1, 2, 3))
  p_no_trend <- plot_decoupling(decoupling_df = df_enough_points, add_trend_line = FALSE, decouple_metric = "Pace_HR")
  expect_false(has_smooth_layer(p_no_trend))
})

test_that("plot_decoupling handles NULL/empty df from internal calculate_decoupling call", {
  # This test checks what happens when decoupling_df is NULL and the internal call
  # to the *real* calculate_decoupling fails (as observed when mocking fails).
  dummy_stoken_for_plot <- structure(list(token = list(access_token = "plot_dummy_error")), class = "Token2.0")

  # We expect calculate_decoupling (when called by plot_decoupling without underlying mocks)
  # to fail trying to fetch activities with a dummy token.
  expect_error(
    plot_decoupling(
      stoken = dummy_stoken_for_plot,
      activity_type = "Ride",
      decouple_metric = "Power_HR"
      # decoupling_df is NULL by default
    ),
    regexp = "Could not fetch any activities."
  )
})

# --- Tests for API path of calculate_decoupling (kept separate, may fail/skip without real token/mocking) ---
# Dummy stoken for API path tests (basic signature checks)
dummy_stoken <- structure(list(token = list(access_token = "dummy")), class = "Token2.0")

test_that("calculate_decoupling (API path) throws error for invalid decouple_metric", {
  expect_error(
    calculate_decoupling(stoken = dummy_stoken, decouple_metric = "Speed_Cadence"),
    # Adjusted regexp to be more flexible with quotes, as they vary by R version/OS
    regexp = "'arg' should be one of .*Pace_HR.*, .*Power_HR.*"
  )
})

test_that("calculate_decoupling (API path) handles non-Token2.0 stoken", {
  expect_error(
    calculate_decoupling(stoken = 12345, decouple_metric = "Power_HR"),
    regexp = "`stoken` must be a valid Token2\\.0 object from rStrava::strava_oauth\\(\\)\\."
  )
})

# Mock for rStrava::get_activity_streams specific to decoupling tests
# Needs to return a dataframe with time, heartrate, and distance/watts
mock_rStrava_get_activity_streams_decoupling <- function(act_data, acts, stoken, types, resolution) {
  # message(sprintf("MOCK get_activity_streams called for index: %d, types: %s", acts, paste(types, collapse=", "))) # Debug
  # Use the existing mock_activity_streams, assuming it has the needed columns
  # In a real scenario, this might need to vary based on 'acts' or types requested
  if(exists("mock_activity_streams")) {
    # Check mock stream has requested types
    if(all(types %in% names(mock_activity_streams))){
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
  # Check mock_activity_list_list is loaded from helper
  expect_true(exists("mock_activity_list_list"), "mock_activity_list_list not loaded from helper.")
  expect_true(exists("mock_activity_streams"), "mock_activity_streams not loaded from helper.") # Needed for stream mock

  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list, # Use the list mock 
    get_activity_streams = mock_rStrava_get_activity_streams_decoupling # Use the stream mock
  )

  # Call calculate_decoupling, triggering the API path
  # Use parameters that ensure the mock activities are filtered and processed
  result_df <- calculate_decoupling(
    stoken = dummy_stoken, # Use the dummy token defined earlier
    activity_type = "Run", # Filter for runs in mock_activity_list_list
    decouple_metric = "Pace_HR",
    start_date = "2023-10-01", 
    end_date = "2023-10-03", # Cover dates in mock data
    min_duration_mins = 20, # Include mock runs (30min, 90min)
    max_activities = 5 # Process all mock activities
  )

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
  # Check structure and count
})

# Test skipped as API call would fail without proper mocking or real token
test_that("calculate_decoupling (API path) structure check (SKIPPED)", {
   skip_on_cran()
   skip("Skipping API call test for calculate_decoupling; requires network or full API mock.")
   # Original test logic commented out as it relies on an actual or complex mocked API call:
   # result_df <- tryCatch({
   #    calculate_decoupling(stoken = dummy_stoken, activity_type = "Run", decouple_metric = "Pace_HR", max_activities = 2, min_duration_mins = 1)
   #  }, error = function(e) { NULL })
   # if (is.null(result_df)) {
   #     skip("Test skipped as API call failed or function returned NULL, likely due to missing mock/real token.")
   # }
   # # else { # If somehow it didn't fail, check structure
   # #   expect_s3_class(result_df, "data.frame")
   # # }
}) 
