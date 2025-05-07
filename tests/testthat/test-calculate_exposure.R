# tests/testthat/test-calculate_exposure.R

library(testthat)
library(Athlytics)
library(mockery)

# Load data: sample data from package & mock API returns from helper
data(Athlytics_sample_data)
source(test_path("helpe-mockdata.R"), local = TRUE) # Provides mock_activity_list_list

# Mock Strava token
mock_stoken <- structure(list(token = list(access_token = "fake_token")), class = "Token2.0")

# --- Test Parameter Validation --- 

test_that("calculate_exposure throws error for invalid load_metric", {
  expect_error(
    calculate_exposure(stoken = mock_stoken, load_metric = "invalid_metric"),
    regexp = "'load_metric' must be one of: duration_mins, distance_km, hrss, tss, elevation_gain_m"
  )
})

test_that("calculate_exposure throws error for missing user_ftp when load_metric is 'tss'", {
  expect_error(
    calculate_exposure(stoken = mock_stoken, load_metric = "tss", user_ftp = NULL),
    regexp = "user_ftp is required when load_metric = 'tss'\\."
  )
})

test_that("calculate_exposure throws error for missing hr parameters when load_metric is 'hrss'", {
  expect_error(
    calculate_exposure(stoken = mock_stoken, load_metric = "hrss", user_max_hr = NULL, user_resting_hr = 50),
    regexp = "user_max_hr and user_resting_hr are required when load_metric = 'hrss'\\."
  )
  expect_error(
    calculate_exposure(stoken = mock_stoken, load_metric = "hrss", user_max_hr = 190, user_resting_hr = NULL),
    regexp = "user_max_hr and user_resting_hr are required when load_metric = 'hrss'\\."
  )
})

# --- Test Calculation Logic with Mocked API ---

test_that("calculate_exposure calculates correctly with mock activity data", {
  # Ensure mock activity list exists
  expect_true(exists("mock_activity_list_list"))

  # Mock the rStrava function called internally
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list
  )

  # Determine end_date from mock data for consistency
  # Note: calculate_exposure needs to fetch data before start_date, 
  # so the effective range in mock_activity_list_list matters.
  # Let's pick a range within the mock data timespan.
  test_end_date <- lubridate::as_date("2023-10-01") 
  test_chronic_period <- 14 # Using shorter period for test based on limited mock data span
  test_acute_period <- 7

  # Call the function under test
  result_df <- calculate_exposure(
    stoken = mock_stoken,        
    activity_type = NULL,         # Use all types from mock
    load_metric = "duration_mins", 
    acute_period = test_acute_period,
    chronic_period = test_chronic_period,
    # start_date is implicitly calculated inside the function based on end_date and chronic_period
    end_date = test_end_date      
  )
  
  # --- Basic Checks ---
  expect_s3_class(result_df, "data.frame")
  # calculate_exposure should return these columns (based on its description)
  expect_named(result_df, c("date", "daily_load", "atl", "ctl", "acwr"), ignore.order = TRUE)
  expect_true(nrow(result_df) > 0)
  expect_s3_class(result_df$date, "Date")
  
  # --- Numerical Checks ---
  expect_true(is.numeric(result_df$daily_load))
  expect_true(is.numeric(result_df$atl))
  expect_true(is.numeric(result_df$ctl))
  expect_true(is.numeric(result_df$acwr))
  
  # Check for non-negative loads (where applicable, some might be NA initially)
  expect_true(all(result_df$daily_load >= 0, na.rm = TRUE))
  expect_true(all(result_df$atl >= 0, na.rm = TRUE))
  expect_true(all(result_df$ctl >= 0, na.rm = TRUE))
  # ACWR can be NA/Inf if ctl is 0
  expect_true(all(is.finite(result_df$acwr) | is.na(result_df$acwr)))
  
}) 