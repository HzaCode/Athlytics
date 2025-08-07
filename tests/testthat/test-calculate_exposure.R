# tests/testthat/test-calculate_exposure.R

library(testthat)
library(athlytics)
library(mockery)

# Load data: sample data from package & mock API returns from helper
data(athlytics_sample_data)
source(test_path("helper-mockdata.R"), local = TRUE)

# Load mock activity list for testing calculate_exposure's internal processing
source(test_path("helper-mockdata.R"), local = TRUE) # Assuming this file defines mock_activity_list_list

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
  # Check mock activity list exists
  expect_true(exists("mock_activity_list_list"))

  # Mock the rStrava function called internally
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list
  )

  # Determine end_date from mock data for consistency
  # Note: calculate_exposure needs to fetch data before start_date, 
  # so the effective range in mock_activity_list_list matters.
  # Pick range within mock data timespan
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

test_that("calculate_exposure works with load_metric = distance_km", {
  expect_true(exists("mock_activity_list_list"))
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list
  )
  test_end_date <- lubridate::as_date("2023-10-01") 
  test_chronic_period <- 14
  test_acute_period <- 7

  result_df <- calculate_exposure(
    stoken = mock_stoken,
    activity_type = NULL, 
    load_metric = "distance_km", # Test distance metric
    acute_period = test_acute_period,
    chronic_period = test_chronic_period,
    end_date = test_end_date
  )
  
  # Basic structure and type checks (similar to duration test)
  expect_s3_class(result_df, "data.frame")
  expect_named(result_df, c("date", "daily_load", "atl", "ctl", "acwr"), ignore.order = TRUE)
  expect_s3_class(result_df$date, "Date")
  expect_true(is.numeric(result_df$daily_load))
  expect_true(is.numeric(result_df$atl))
  expect_true(is.numeric(result_df$ctl))
  expect_true(is.numeric(result_df$acwr))
  # Check if daily_load values seem plausible based on mock distance data (mock has distances like 5050m, 20100m etc.)
  # Example: Expect some loads around 5, 10, 20 etc.
  expect_true(any(result_df$daily_load > 1))
  expect_true(all(result_df$daily_load >= 0, na.rm = TRUE))
})

test_that("calculate_exposure works with load_metric = elevation_gain_m", {
  expect_true(exists("mock_activity_list_list"))
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list
  )
  test_end_date <- lubridate::as_date("2023-10-01")
  test_chronic_period <- 14
  test_acute_period <- 7

  result_df <- calculate_exposure(
    stoken = mock_stoken,
    activity_type = NULL,
    load_metric = "elevation_gain_m", # Test elevation metric
    acute_period = test_acute_period,
    chronic_period = test_chronic_period,
    end_date = test_end_date
  )

  # Basic structure and type checks
  expect_s3_class(result_df, "data.frame")
  expect_named(result_df, c("date", "daily_load", "atl", "ctl", "acwr"), ignore.order = TRUE)
  expect_s3_class(result_df$date, "Date")
  expect_true(is.numeric(result_df$daily_load))
  expect_true(is.numeric(result_df$atl))
  expect_true(is.numeric(result_df$ctl))
  expect_true(is.numeric(result_df$acwr))
  # Check if daily_load values seem plausible (mock has elevation like 50, 150, 100, 250 etc.)
  expect_true(any(result_df$daily_load > 10))
  expect_true(all(result_df$daily_load >= 0, na.rm = TRUE))
})

test_that("calculate_exposure works with load_metric = hrss", {
  expect_true(exists("mock_activity_list_list"))
  # Check mock data has average_heartrate
  expect_true(any(sapply(mock_activity_list_list, function(x) !is.null(x$average_heartrate) && !is.na(x$average_heartrate))))
  
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list
  )
  test_end_date <- lubridate::as_date("2023-10-01")
  test_chronic_period <- 14
  test_acute_period <- 7

  result_df <- calculate_exposure(
    stoken = mock_stoken,
    activity_type = NULL,
    load_metric = "hrss", # Test HRSS metric
    user_max_hr = 190, # Provide necessary HR parameters
    user_resting_hr = 50,
    acute_period = test_acute_period,
    chronic_period = test_chronic_period,
    end_date = test_end_date
  )

  # Basic structure and type checks
  expect_s3_class(result_df, "data.frame")
  expect_named(result_df, c("date", "daily_load", "atl", "ctl", "acwr"), ignore.order = TRUE)
  expect_s3_class(result_df$date, "Date")
  expect_true(is.numeric(result_df$daily_load))
  expect_true(is.numeric(result_df$atl))
  expect_true(is.numeric(result_df$ctl))
  expect_true(is.numeric(result_df$acwr))
  # HRSS values can vary widely, just check if calculation produced non-zero loads
  expect_true(any(result_df$daily_load > 0))
  expect_true(all(result_df$daily_load >= 0, na.rm = TRUE))
  
  # Test edge case: invalid HR parameters should ideally not produce load
  mock_list_hrss_edge <- list(list(
    id = 1, name = "Run with HR", type = "Run", start_date_local = "2023-10-01T08:00:00Z",
    distance = 5000, moving_time = 1800, elapsed_time = 1850, average_heartrate = 150,
    total_elevation_gain = 50, average_watts = NA, start_date="2023-10-01T08:00:00Z", sport_type="Run"
  ))
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_list_hrss_edge
  )
  # Case 1: HR below resting HR - Expect error because no valid load activities found
  expect_error(
    calculate_exposure(stoken = mock_stoken, load_metric = "hrss", user_max_hr=190, user_resting_hr=160, end_date = "2023-10-01"),
    regexp = "No activities with valid load data found"
  )
  
  # Case 2: HR above max HR (should still calc if possible? The formula uses (avg-rest)/(max-rest))
  # The code doesn't explicitly filter avg_hr > max_hr, it relies on user inputs being sensible
  
  # Case 3: max_hr <= resting_hr (division by zero or negative) - Expect error because no valid load activities found
  expect_error(
    calculate_exposure(stoken = mock_stoken, load_metric = "hrss", user_max_hr=50, user_resting_hr=50, end_date = "2023-10-01"),
    regexp = "No activities with valid load data found"
  )
})

test_that("calculate_exposure works with load_metric = tss", {
  expect_true(exists("mock_activity_list_list"))
  # Check mock data has average_watts (used as proxy for NP if device_watts is null)
  expect_true(any(sapply(mock_activity_list_list, function(x) !is.null(x$average_watts) && !is.na(x$average_watts))))

  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list
  )
  test_end_date <- lubridate::as_date("2023-10-01")
  test_chronic_period <- 14
  test_acute_period <- 7

  result_df <- calculate_exposure(
    stoken = mock_stoken,
    activity_type = NULL,
    load_metric = "tss", # Test TSS metric
    user_ftp = 250,      # Provide necessary FTP parameter
    acute_period = test_acute_period,
    chronic_period = test_chronic_period,
    end_date = test_end_date
  )

  # Basic structure and type checks
  expect_s3_class(result_df, "data.frame")
  expect_named(result_df, c("date", "daily_load", "atl", "ctl", "acwr"), ignore.order = TRUE)
  expect_s3_class(result_df$date, "Date")
  expect_true(is.numeric(result_df$daily_load))
  expect_true(is.numeric(result_df$atl))
  expect_true(is.numeric(result_df$ctl))
  expect_true(is.numeric(result_df$acwr))
  # TSS values can vary, check if calculation produced non-zero loads for activities with power
  expect_true(any(result_df$daily_load > 0))
  expect_true(all(result_df$daily_load >= 0, na.rm = TRUE))
  
  # Test edge case: invalid FTP or power should not produce load
  mock_list_tss_edge <- list(list(
    id = 2, name = "Ride with Power", type = "Ride", start_date_local = "2023-10-01T18:00:00Z",
    distance = 20000, moving_time = 3600, elapsed_time = 3700, average_watts = 200,
    total_elevation_gain = 150, average_heartrate = 150, start_date="2023-10-01T18:00:00Z", sport_type="Ride"
  ), list(
    id = 4, name = "Ride with Zero Power", type = "Ride", start_date_local = "2023-09-25T08:00:00Z",
    distance = 30500, moving_time = 5400, elapsed_time = 5500, average_watts = 0,
    total_elevation_gain = 250, average_heartrate = 145, start_date="2023-09-25T08:00:00Z", sport_type="Ride"
  ))
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_list_tss_edge
  )
  # Case 1: FTP is zero - Expect error because no valid load activities found
  expect_error(
    calculate_exposure(stoken = mock_stoken, load_metric = "tss", user_ftp = 0, end_date = "2023-10-01"),
    regexp = "No activities with valid load data found"
  )
  
  # Case 2: Average power is zero for one activity
  result_zero_power <- calculate_exposure(stoken = mock_stoken, load_metric = "tss", user_ftp = 250, end_date = "2023-10-01")
  # We expect the activity with zero power to contribute zero load.
  # The activity with positive power should contribute positive load.
  expect_true(all(result_zero_power$daily_load >= 0)) # Non-negative values
  expect_true(any(result_zero_power$daily_load > 0)) # At least one day has positive load
}) 