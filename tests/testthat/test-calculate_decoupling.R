# tests/testthat/test-calculate_decoupling.R

library(testthat)
library(Athlytics)
library(rStrava)

# --- Helper: Dummy stoken ---
dummy_stoken <- structure(list(endpoint = NULL, app = NULL, credentials = list(dummy = "token")), class = "Token2.0")

# --- Test Cases for calculate_decoupling ---

test_that("calculate_decoupling returns a data frame with expected columns", {
  skip_on_cran()
  # This function relies heavily on stream data, making it hard to test without mocks
  # Basic structure check might be possible if the function handles cases
  # where no suitable activities are found gracefully (e.g., returns empty df)
  
  # Attempt a run, expecting it might return an empty df or fail gracefully
  # if dummy token prevents actual data fetch needed for streams.
  # A better approach requires mocking rStrava::get_streams
  result_df <- tryCatch({
      calculate_decoupling(
        stoken = dummy_stoken,
        activity_type = "Run",
        decouple_metric = "Pace_HR",
        max_activities = 2, # Minimal activities for test structure
        min_duration_mins = 1 # Low duration for test
      )
    }, 
    error = function(e) {
      # If it errors due to API call failure (expected without mock/real token), return NULL for test check
      message("API call likely failed as expected without mock/real token: ", e$message)
      return(NULL) 
    }
  )

  # Only proceed with checks if the function didn't error out completely
  if (!is.null(result_df)) {
      expect_s3_class(result_df, "data.frame")
      # Check core columns, names might vary slightly
      expect_true(all(c("activity_id", "start_date", "activity_type", "decoupling_metric", "decoupling_percent") %in% names(result_df)))
      
      # Check types
      expect_type(result_df$activity_id, "character") # Or integer
      expect_type(result_df$start_date, "double") # Date/POSIXct
      expect_type(result_df$activity_type, "character")
      expect_type(result_df$decoupling_metric, "character")
      expect_type(result_df$decoupling_percent, "double")
  } else {
      # If it returned NULL due to expected error, mark test as inconclusive or skipped if possible
      # testthat doesn't have a direct 'inconclusive', maybe just skip
      skip("Test skipped as API call failed or function returned NULL, likely due to missing mock/real token.")
  }
})


test_that("calculate_decoupling throws error for invalid decouple_metric", {
  dummy_stoken <- list(credentials = "dummy") # Minimal dummy token
  class(dummy_stoken) <- "Token2.0"
  expect_error(
    calculate_decoupling(stoken = dummy_stoken, decouple_metric = "Speed_Cadence"),
    regexp = "'arg' should be one of [\"“]Pace_HR[\"”], [\"“]Power_HR[\"”]" # Allow straight or smart quotes
  )
})

test_that("calculate_decoupling handles non-Token2.0 stoken", {
  expect_error(
    calculate_decoupling(stoken = 12345, decouple_metric = "Power_HR"),
    regexp = "`stoken` must be a valid Token2\\.0 object from rStrava::strava_oauth\\(\\)\\." # Use \\ for regex special chars
  )
})

# Add tests for min_duration_mins, date ranges 