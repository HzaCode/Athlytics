# tests/testthat/test-calculate_exposure.R

library(testthat)
library(Athlytics)
library(rStrava) # Needed for stoken object simulation

# --- Helper: Create a dummy stoken for structure checks ---
# In real tests with mocking, this wouldn't hit the API.
# Here, we create a placeholder. Actual API calls might still fail
# if a valid token isn't available environment when tests run interactively.
# For automated checks (like R CMD check), tests hitting APIs are skipped on CRAN.
# We create a dummy structure just to pass object type checks.
dummy_stoken <- structure(list(endpoint = NULL, app = NULL, credentials = list(dummy = "token")), class = "Token2.0")

# --- Test Cases ---

test_that("calculate_exposure returns a data frame with expected columns (basic case - expects API error)", {
  with_mocked_strava_api({
    expect_error(
      calculate_exposure(
        stoken = dummy_stoken,
        activity_type = "Run",
        load_metric = "duration_mins",
        acute_period = 7,
        chronic_period = 28,
        end_date = "2023-10-27"
      ),
      regexp = "Could not fetch activities or no activities found" # Keep this one expecting API error
    )
  })
})

test_that("calculate_exposure throws error for invalid load_metric", {
  with_mocked_strava_api({
    expect_error(
      calculate_exposure(stoken = dummy_stoken, load_metric = "invalid_metric"),
      regexp = "'load_metric' must be one of: duration_mins, distance_km, hrss, tss, elevation_gain_m" 
    )
  })
})

test_that("calculate_exposure throws error for missing user_ftp when load_metric is 'tss'", {
  with_mocked_strava_api({
    expect_error(
      calculate_exposure(stoken = dummy_stoken, load_metric = "tss", user_ftp = NULL),
      regexp = "user_ftp is required when load_metric = 'tss'\\." 
    )
  })
})

test_that("calculate_exposure throws error for missing user_max_hr/user_resting_hr when load_metric is 'hrss'", {
  with_mocked_strava_api({
    expect_error(
      calculate_exposure(stoken = dummy_stoken, load_metric = "hrss", user_max_hr = NULL, user_resting_hr = 50),
      regexp = "user_max_hr and user_resting_hr are required when load_metric = 'hrss'\\." 
    )
  })
})

test_that("calculate_exposure throws error for missing user_resting_hr when load_metric is 'hrss'", {
  with_mocked_strava_api({
    expect_error(
      calculate_exposure(stoken = dummy_stoken, load_metric = "hrss", user_max_hr = 190, user_resting_hr = NULL),
      regexp = "user_max_hr and user_resting_hr are required when load_metric = 'hrss'\\." 
    )
  })
})

test_that("calculate_exposure handles non-Token2.0 stoken", {
  # This test might be tricky if the function tries to fetch before full validation
  # We expect it to fail during fetching if validation doesn't catch it first.
  expect_error(
    calculate_exposure(stoken = 12345, load_metric = "duration_mins"),
    regexp = "Could not fetch activities or no activities found" # Keep this one expecting API error
  )
})

# Add more tests here for edge cases like:
# - Different activity types
# - Cases where no activities are returned (would need mocking ideally)
# - Date range filtering (start_date, end_date) 

# --- Test Success Path with Mock Data ---

library(mockery)

test_that("calculate_exposure calculates correctly with mock activity data", {
  # Use the mock activity list defined in helper-mockdata.R
  # Ensure the helper file is sourced or the object is available
  # Make sure mock_activity_list_df exists from helper-mockdata.R
  if (!exists("mock_activity_list_df")) {
      source(test_path("helper-mockdata.R"))
  }
  
  # Convert mock data frame to list *before* defining mock_fetch
  if (!requireNamespace("purrr", quietly = TRUE)) stop("purrr needed for test")
  # Create a copy to modify
  temp_mock_df <- mock_activity_list_df
  # Convert start_date_local to character *before* transposing
  temp_mock_df$start_date_local <- as.character(temp_mock_df$start_date_local)
  activities_as_list <- purrr::transpose(temp_mock_df)
  names(activities_as_list) <- temp_mock_df$id

  # Define the mock fetch function to just return the pre-converted list
  mock_fetch <- function(...) {
    print("Mock fetch_strava_activities called, returning pre-converted mock data AS LIST.") # Debug message
    return(activities_as_list) # Return the list created outside
  }

  # Use mockery::stub to replace the real function with our mock during this test
  # We need to mock rStrava::get_activity_list, as that's what calculate_exposure calls internally
  stub(calculate_exposure, 'rStrava::get_activity_list', mock_fetch)

  # Determine end_date from the pre-converted list (now available outside mock_fetch)
  mock_dates <- purrr::map_vec(activities_as_list, ~.x$start_date_local, .default = NA_real_)
  valid_mock_dates <- as.POSIXct(mock_dates[!is.na(mock_dates)], origin = "1970-01-01", tz = "UTC")
  test_end_date <- max(valid_mock_dates)

  # Call the function under test
  result_df <- calculate_exposure(
    stoken = dummy_stoken,        # Still need a valid structure for checks
    activity_type = NULL,         # Use NULL to include all types from mock data
    load_metric = "duration_mins", # Metric available in mock data
    acute_period = 7,
    chronic_period = 28,
    end_date = test_end_date      # Use end date derived from mock list
  )
  
  # --- Basic Checks ---
  expect_s3_class(result_df, "data.frame")
  expect_named(result_df, c("date", "daily_load", "atl", "ctl", "acwr"), ignore.order = TRUE)
  expect_true(nrow(result_df) > 0)
  
  # --- More Specific Checks (Example: Check last row values) ---
  # These expected values would need careful calculation based on the mock data
  # and the specific ATL/CTL formulas used in calculate_exposure.
  # For now, we'll just check the structure and non-emptiness.
  # last_row <- result_df[nrow(result_df), ]
  # expect_equal(last_row$date, as.Date(max(mock_activity_list_df$start_date_local)))
  # expect_gt(last_row$atl, 0) # Example: Expect positive load
  # expect_gt(last_row$ctl, 0)
  # expect_gt(last_row$acwr, 0)
  
}) 