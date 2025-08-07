# tests/testthat/test-acwr.R

context("ACWR Calculation and Plotting")

library(athlytics)
library(testthat)
library(mockery)

# Load sample data from the package
data(athlytics_sample_data)

# Load mock activity list for testing calculate_acwr's internal processing
# This specific mock is for simulating rStrava::get_activity_list output
# Keep this source line if mock_activity_list_list is not part of athlytics_sample_data
# and is essential for testing the raw calculation logic of calculate_acwr.
  # Note: helper-mockdata.R contains raw API mocks
# or integrate such mocks into a different structure if athlytics_sample_data contains only processed data.
source(test_path("helper-mockdata.R"), local = TRUE) # Assuming this file name is correct and it defines mock_activity_list_list

# Mock Strava token - use structure() to correctly set class attributes
mock_stoken <- structure(
  list(
    token = list(
      access_token = "fake_access_token",
      token_type = "Bearer",
      expires_at = as.integer(Sys.time() + 3600),
      refresh_token = "fake_refresh_token"
    )
  ),
  class = c("Token2.0", "Token")
)

# Define test date range
test_start_date <- "2023-09-13"
test_end_date <- "2023-10-01"
expected_rows <- as.numeric(lubridate::ymd(test_end_date) - lubridate::ymd(test_start_date)) + 1

# --- Test calculate_acwr --- 

test_that("calculate_acwr output structure and basic numeric checks with mocked API", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list # Uses list of lists from helpe-mockdata.R
  )

  acwr_duration <- calculate_acwr(
    stoken = mock_stoken,
    load_metric = "duration_mins",
    activity_type = "Run", # Specify activity_type for filtering within calculate_acwr
    acute_period = 7,
    chronic_period = 14,
    start_date = test_start_date,
    end_date = test_end_date
  )

  # Structure checks
  expect_s3_class(acwr_duration, "data.frame")
  expect_true(all(c("date", "atl", "ctl", "acwr", "acwr_smooth") %in% colnames(acwr_duration)))
  expect_s3_class(acwr_duration$date, "Date")
  # The number of rows might vary based on actual mock data and filtering, adjust if necessary
  # For this specific mocked data, the date range is small, so expected_rows might be accurate.
  # However, calculate_acwr also fetches data prior to start_date for CTL calculation.
  # The final output is filtered for the analysis_start_date to analysis_end_date.
  expect_equal(nrow(acwr_duration), expected_rows) 

  # Numerical type and range checks (remove NA or Inf values)
  atl_valid <- acwr_duration$atl[!is.na(acwr_duration$atl)]
  ctl_valid <- acwr_duration$ctl[!is.na(acwr_duration$ctl)]
  acwr_valid <- acwr_duration$acwr[!is.na(acwr_duration$acwr) & is.finite(acwr_duration$acwr)]
  acwr_smooth_valid <- acwr_duration$acwr_smooth[!is.na(acwr_duration$acwr_smooth) & is.finite(acwr_duration$acwr_smooth)]

  expect_true(is.numeric(atl_valid))
  expect_true(is.numeric(ctl_valid))
  expect_true(is.numeric(acwr_valid))
  expect_true(is.numeric(acwr_smooth_valid))

  # Check if calculations produce non-negative values (where applicable)
  if(length(atl_valid) > 0) expect_true(all(atl_valid >= 0))
  if(length(ctl_valid) > 0) expect_true(all(ctl_valid >= 0))
  # ACWR can be positive or NA/Inf if CTL is zero, already handled by is.finite
})

test_that("calculate_acwr input parameter validations", {
  # Existing invalid stoken test
  expect_error(calculate_acwr(stoken = "invalid"), ".*Token2.0 object.*")

  # Existing acute_period vs chronic_period test (ensure mock_stoken is used here)
  expect_error(
    calculate_acwr(
      stoken = mock_stoken,
      acute_period = 28,
      chronic_period = 7,
      start_date = test_start_date,
      end_date = test_end_date
    ),
    message = "`acute_period` must be less than `chronic_period`."
  )

  # New tests
  expect_error(
    calculate_acwr(stoken = mock_stoken, acute_period = -1),
    message = "`acute_period` must be a positive integer."
  )
  expect_error(
    calculate_acwr(stoken = mock_stoken, chronic_period = 0),
    message = "`chronic_period` must be a positive integer."
  )
  expect_error(
    calculate_acwr(stoken = mock_stoken, load_metric = "tss", user_ftp = NULL),
    message = "`user_ftp` is required when `load_metric` is 'tss'."
  )
  expect_error(
    calculate_acwr(stoken = mock_stoken, load_metric = "hrss", user_max_hr = NULL, user_resting_hr = 100),
    message = "`user_max_hr` and `user_resting_hr` are required when `load_metric` is 'hrss'."
  )
  expect_error(
    calculate_acwr(stoken = mock_stoken, load_metric = "hrss", user_max_hr = 200, user_resting_hr = NULL),
    message = "`user_max_hr` and `user_resting_hr` are required when `load_metric` is 'hrss'."
  )
  expect_error(
    calculate_acwr(stoken = mock_stoken, load_metric = "invalid_metric"),
    message = "Invalid `load_metric`."
  )
  expect_error(
    calculate_acwr(stoken = mock_stoken, start_date = "2023-10-01", end_date = "2023-09-01"),
    message = "start_date must be before end_date."
  )
})

test_that("calculate_acwr works with load_metric = distance_km", {
  expect_true(exists("mock_activity_list_list"))
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list
  )

  result_df <- calculate_acwr(
    stoken = mock_stoken,
    load_metric = "distance_km", # Test distance metric
    activity_type = NULL, 
    acute_period = 7,
    chronic_period = 14, # Use shorter periods for test
    start_date = test_start_date,
    end_date = test_end_date
  )
  
  # Basic structure and type checks
  expect_s3_class(result_df, "data.frame")
  expect_named(result_df, c("date", "atl", "ctl", "acwr", "acwr_smooth"), ignore.order = TRUE)
  expect_s3_class(result_df$date, "Date")
  expect_true(is.numeric(result_df$atl))
  expect_true(is.numeric(result_df$ctl))
  # Add check for daily_load presence if it were returned, but it's not in the final output
})

test_that("calculate_acwr works with load_metric = hrss", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list
  )

  # Scenario 1: Valid HRSS calculation
  acwr_hrss_valid <- calculate_acwr(
    stoken = mock_stoken,
    load_metric = "hrss",
    activity_type = "Run", # Mock data has HR for Runs
    user_max_hr = 180,
    user_resting_hr = 60,
    start_date = test_start_date,
    end_date = test_end_date,
    acute_period = 7,
    chronic_period = 14
  )
  expect_s3_class(acwr_hrss_valid, "data.frame")
  expect_true(all(c("atl", "ctl", "acwr") %in% colnames(acwr_hrss_valid)))
  # Check that some load was calculated (atl/ctl won't be all zero or NA)
  # Given mock data, some activities should have valid HR to calculate HRSS
  expect_true(any(acwr_hrss_valid$atl > 0, na.rm = TRUE) || any(acwr_hrss_valid$ctl > 0, na.rm = TRUE))

  # Scenario 2: Invalid HR parameters (e.g., max_hr <= resting_hr)
  # Results in calculation messages 
  # and potentially 0 load for activities where HRSS cannot be calculated.
  # The overall ACWR calculation should still run, possibly with more NAs or 0s in load.
  expect_error(
    calculate_acwr(
      stoken = mock_stoken,
      load_metric = "hrss",
      user_max_hr = 60, # Invalid: max_hr <= resting_hr
      user_resting_hr = 70,
      start_date = test_start_date,
      end_date = test_end_date
    ),
    regexp = "No activities found with valid load data for the specified criteria."
  )
})

test_that("calculate_acwr works with load_metric = tss", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list 
  )

  # Scenario 1: Valid TSS calculation (for activities with power data)
  acwr_tss_valid <- calculate_acwr(
    stoken = mock_stoken,
    load_metric = "tss",
    activity_type = "Ride", # Mock data has watts for some Rides
    user_ftp = 250,
    start_date = test_start_date,
    end_date = test_end_date,
    acute_period = 7,
    chronic_period = 14
  )
  expect_s3_class(acwr_tss_valid, "data.frame")
  expect_true(all(c("atl", "ctl", "acwr") %in% colnames(acwr_tss_valid)))
  # Some rides in mock data have average_watts, so TSS should be calculable for them.
  expect_true(any(acwr_tss_valid$atl > 0, na.rm = TRUE) || any(acwr_tss_valid$ctl > 0, na.rm = TRUE))

  # Scenario 2: FTP is NULL (should error, but covered by input validation)
  # Scenario 3: FTP is <= 0 (should result in 0 load or issues, similar to invalid HRSS)
  expect_error(
    calculate_acwr(
      stoken = mock_stoken,
      load_metric = "tss",
      user_ftp = 0, # Invalid FTP
      start_date = test_start_date,
      end_date = test_end_date
    ),
    regexp = "No activities found with valid load data for the specified criteria."
  )
})

test_that("calculate_acwr works with load_metric = elapsed_time_mins", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list
  )
  acwr_elapsed_time <- calculate_acwr(
    stoken = mock_stoken,
    load_metric = "elapsed_time_mins",
    start_date = test_start_date,
    end_date = test_end_date
  )
  expect_s3_class(acwr_elapsed_time, "data.frame")
  expect_true(all(c("atl", "ctl", "acwr") %in% colnames(acwr_elapsed_time)))
  expect_true(any(acwr_elapsed_time$atl > 0, na.rm = TRUE) || any(acwr_elapsed_time$ctl > 0, na.rm = TRUE))
})

test_that("calculate_acwr works with load_metric = elevation_gain_m", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list
  )
  acwr_elevation <- calculate_acwr(
    stoken = mock_stoken,
    load_metric = "elevation_gain_m",
    start_date = test_start_date,
    end_date = test_end_date
  )
  expect_s3_class(acwr_elevation, "data.frame")
  expect_true(all(c("atl", "ctl", "acwr") %in% colnames(acwr_elevation)))
  # Elevation gain can be 0 for some activities, but some should be > 0 in mock data
  expect_true(any(acwr_elevation$atl > 0, na.rm = TRUE) || any(acwr_elevation$ctl > 0, na.rm = TRUE))
})

test_that("calculate_acwr handles empty activity list from API", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) list() # Simulate API returning no activities
  )
  expect_error(
    calculate_acwr(
      stoken = mock_stoken,
      start_date = test_start_date,
      end_date = test_end_date
    ),
    regexp = "Could not fetch activities or no relevant activities found"
  )
})

test_that("calculate_acwr handles no valid load data after processing activities", {
  # Simulate activities that will all result in zero or NA load
  # For example, by filtering all out by type, or making all durations zero.
  # Here, we'll use a non-matching activity_type with the existing mock_activity_list_list
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list
  )
  expect_error(
    calculate_acwr(
      stoken = mock_stoken,
      activity_type = "NonExistentType", # This type is not in mock_activity_list_list
      start_date = test_start_date,
      end_date = test_end_date
    ),
    regexp = "No activities found with valid load data for the specified criteria."
  )

  # Another scenario: activities exist but all have zero load (e.g. all moving_time = 0)
  # Create a custom mock list for this specific scenario
  mock_activities_zero_load_list <- lapply(mock_activity_list_list, function(act) {
    act$moving_time <- 0
    act$elapsed_time <- 0 # also make elapsed time 0 for elapsed_time_mins
    act$distance <- 0 # also for distance_km
    act$total_elevation_gain <- 0 # for elevation_gain_m
    # For HRSS/TSS, if moving_time is 0, load should be 0.
    return(act)
  })

  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activities_zero_load_list
  )
  expect_error(
    calculate_acwr(
      stoken = mock_stoken,
      load_metric = "duration_mins", # or any other metric
      start_date = test_start_date,
      end_date = test_end_date
    ),
    regexp = "No activities found with valid load data for the specified criteria."
  )
})

# --- Test plot_acwr --- 

test_that("plot_acwr returns a ggplot object using pre-calculated athlytics_sample_data", {
  # Check athlytics_sample_data is loaded and contains athlytics_sample_acwr
  expect_true(exists("athlytics_sample_acwr"))
  expect_s3_class(athlytics_sample_acwr, "data.frame")
  
  # Assuming plot_acwr uses 'load_metric' for plot labels even with pre-calculated df.
  # Provide a relevant activity_type if it influences plot aesthetics or default calculations if df was NULL.
  p <- plot_acwr(acwr_df = athlytics_sample_acwr, load_metric = "duration_mins", activity_type = "Run") 
  expect_s3_class(p, "ggplot")
  # Check if the plot data contains the key column used for y-axis
  expect_true("acwr_smooth" %in% names(p$data))
})

# This test verifies the branch where plot_acwr calls calculate_acwr internally
test_that("plot_acwr correctly calls calculate_acwr and generates plot with mocked API", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list 
  )

  p <- plot_acwr(
    stoken = mock_stoken,
    activity_type = "Run",
    load_metric = "duration_mins",
    start_date = test_start_date,
    end_date = test_end_date,
    acute_period = 7,
    chronic_period = 14 # Periods consistent with calculate_acwr test
  )
  expect_s3_class(p, "ggplot")
  # Further checks on p$data could be done here, e.g., expected columns from calculate_acwr
  expect_true(all(c("date", "atl", "ctl", "acwr", "acwr_smooth") %in% colnames(p$data)))
})
