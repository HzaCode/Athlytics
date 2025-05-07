# tests/testthat/test-calculate_decoupling.R

library(testthat)
library(Athlytics)
library(mockery)
# library(rStrava) # Not calling real rStrava functions in this test file

# Load data: sample data from package & mock API returns from helper
data(Athlytics_sample_data)
source(test_path("helpe-mockdata.R"), local = TRUE) # Provides mock_activity_list_list, mock_activity_streams

# Mock Strava token - needed for function signature but API calls will be mocked
mock_stoken <- structure(
  list(token = list(access_token = "fake_token")), class = "Token2.0"
)

# --- Test calculate_decoupling (API Path Logic with Mocking) ---

test_that("calculate_decoupling (API path) processes mocked data correctly", {
  
  # Check if necessary mock objects exist
  expect_true(exists("mock_activity_list_list"), "mock_activity_list_list not found in helper.")
  expect_true(exists("mock_activity_streams"), "mock_activity_streams not found in helper.")
  
  # Mock the rStrava functions called internally by calculate_decoupling
  local_mocked_bindings(
    .package = "rStrava",
    # Mock getting the list of activities
    get_activity_list = function(stoken, ...) {
      # message("MOCK get_activity_list called") # Optional debug message
      return(mock_activity_list_list) 
    },
    # Mock getting the streams for ANY activity ID requested
    # We return the *same* mock stream data regardless of the ID requested,
    # simplifying the test while still checking the processing logic.
    get_activity_streams = function(act_data, acts, stoken, types, resolution, ...) {
      # message(sprintf("MOCK get_activity_streams called for index %d", acts)) # Optional debug message
      # Input validation based on expected types from calculate_decoupling
      expect_true(is.list(act_data))
      expect_true(is.numeric(acts))
      expect_s3_class(stoken, "Token2.0")
      expect_true(is.character(types))
      # Return the standard mock stream data
      return(mock_activity_streams)
    }
  )

  # Define parameters for the test run
  test_max_activities <- 3 # Keep low for faster test
  test_start_date <- "2023-09-25" # Ensure dates overlap with mock_activity_list_list
  test_end_date <- "2023-10-01"
  
  # Call the function under test with the mocked environment
  result_df <- calculate_decoupling(
    stoken = mock_stoken,
    activity_type = "Run", # Filter activities based on mock_activity_list_list
    decouple_metric = "Pace_HR",
    start_date = test_start_date,
    end_date = test_end_date,
    max_activities = test_max_activities,
    min_duration_mins = 1 # Low duration to likely include mock activities
  )

  # Assertions on the result
  expect_s3_class(result_df, "data.frame")
  expect_named(result_df, c("date", "decoupling"), ignore.order = TRUE)
  expect_s3_class(result_df$date, "Date")
  expect_type(result_df$decoupling, "double")
  
  # Check number of rows (should be <= max_activities, depending on filtering)
  # Need to know how many 'Run' activities are in mock_activity_list_list within the date range
  # Let's check it's not empty and plausible
  expect_gt(nrow(result_df), 0)
  expect_lte(nrow(result_df), test_max_activities)
  
  # Check if decoupling values are finite
  expect_true(all(is.finite(result_df$decoupling)))
  
})

# Optional: Add tests for edge cases if needed, e.g., 
# - what happens if get_activity_list returns empty list? 
# - what happens if get_activity_streams returns NULL? (Mock it to return NULL)
# - what happens if all activities are filtered out?

test_that("calculate_decoupling handles empty activity list from mock", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) list() # Return empty list
  )
  expect_error(calculate_decoupling(stoken = mock_stoken), 
                 regexp = "Could not fetch any activities.")
})

test_that("calculate_decoupling handles NULL stream return from mock", {
    local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list, # Return some activities
    get_activity_streams = function(...) NULL # Simulate stream fetch failure
  )
  # Expecting the function to potentially error out after finding no results
  expect_error(calculate_decoupling(stoken = mock_stoken, max_activities = 1), 
                 regexp = "Could not calculate decoupling for any activities after fetching streams.") 
                 # Message might vary, adjust regexp as needed
})

# Add tests for min_duration_mins, date ranges 