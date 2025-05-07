# tests/testthat/test-decoupling.R

library(testthat)
library(Athlytics)
# library(rStrava) # No longer needed directly if we use stream_df or Athlytics_sample_data

# Load main sample data for the package
data(Athlytics_sample_data)

# Load helper mock data (specifically for mock_activity_streams if still needed)
# Consider minimizing this dependency or integrating mock_activity_streams if appropriate
source(test_path("helpe-mockdata.R"), local = TRUE) # Assuming this defines mock_activity_streams

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

# --- Test Cases for plot_decoupling (using Athlytics_sample_data) ---

test_that("plot_decoupling returns a ggplot object using Athlytics_sample_data", {
  expect_true(exists("athlytics_sample_decoupling"), "athlytics_sample_decoupling not found in Athlytics_sample_data.")
  expect_s3_class(athlytics_sample_decoupling, "data.frame")
  # Ensure required columns for plotting are present in the sample data
  expect_true(all(c("date", "decoupling") %in% names(athlytics_sample_decoupling)))
  
  # plot_decoupling requires decouple_metric for labeling if not directly derivable from decoupling_df
  # We assume athlytics_sample_decoupling might be for a mix or needs this specified.
  p_decouple <- plot_decoupling(decoupling_df = athlytics_sample_decoupling, decouple_metric = "Pace_HR")
  expect_s3_class(p_decouple, "ggplot")
  
  # Check that the plot is not empty / uses the data
  expect_gt(nrow(p_decouple$data), 0)
  expect_true("decoupling" %in% names(p_decouple$data))
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