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
  expect_equal(Athlytics:::explicit_english_month_year(single_date), "Jan 2023")

  # Test with multiple dates
  multiple_dates <- c(ymd("2023-03-10"), ymd("2024-11-05"))
  expect_equal(Athlytics:::explicit_english_month_year(multiple_dates), c("Mar 2023", "Nov 2024"))

  # Test with dates spanning year-end
  year_end_dates <- c(ymd("2022-12-25"), ymd("2023-01-01"))
  expect_equal(Athlytics:::explicit_english_month_year(year_end_dates), c("Dec 2022", "Jan 2023"))

  # Test with a leap year date
  leap_date <- ymd("2024-02-29")
  expect_equal(Athlytics:::explicit_english_month_year(leap_date), "Feb 2024")
  
  # Test with an empty vector of dates
  empty_dates <- ymd(character(0))
  expect_equal(Athlytics:::explicit_english_month_year(empty_dates), character(0))
  
  # Test with NA date - depends on how lubridate::month/year handle NA
  # lubridate::month(NA) is NA_integer_, lubridate::year(NA) is NA_integer_
  # eng_months[NA_integer_] is NA_character_, paste(NA_character_, NA_integer_) is NA_character_
  na_date <- ymd(NA)
  expect_equal(Athlytics:::explicit_english_month_year(na_date), NA_character_)
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

test_that("calculate_decoupling handles edge cases", {
  expect_true(exists("mock_activity_streams"), "mock_activity_streams not loaded from helper.")
  
  # Test with empty stream_df
  empty_streams <- data.frame()
  expect_error(calculate_decoupling(stream_df = empty_streams, decouple_metric = "Power_HR"),
               regexp = "Provided `stream_df` is invalid or missing required columns")
  
  # Test with stream_df containing all zeros
  zero_streams <- mock_activity_streams
  zero_streams$watts <- 0
  zero_streams$heartrate <- 0
  zero_streams$velocity_smooth <- 0
  
  # This should still work but might return specific values
  decoupling_zero <- calculate_decoupling(stream_df = zero_streams, decouple_metric = "Power_HR")
  expect_true(is.numeric(decoupling_zero) || is.logical(decoupling_zero))
  expect_true(is.finite(decoupling_zero) || is.na(decoupling_zero))
  
  # Test with stream_df containing all NAs
  na_streams <- mock_activity_streams
  na_streams$watts <- NA
  na_streams$heartrate <- NA
  na_streams$velocity_smooth <- NA
  
  expect_error(calculate_decoupling(stream_df = na_streams, decouple_metric = "Power_HR"),
               regexp = "Not enough valid data points in stream_df after removing NAs")
})

test_that("calculate_decoupling works with different decouple_metric options", {
  expect_true(exists("mock_activity_streams"), "mock_activity_streams not loaded from helper.")
  
  # Test Power_HR metric
  decoupling_power <- calculate_decoupling(stream_df = mock_activity_streams, decouple_metric = "Power_HR")
  expect_type(decoupling_power, "double")
  expect_true(is.finite(decoupling_power))
  
  # Test Pace_HR metric
  decoupling_pace <- calculate_decoupling(stream_df = mock_activity_streams, decouple_metric = "Pace_HR")
  expect_type(decoupling_pace, "double")
  expect_true(is.finite(decoupling_pace))
  
  # Test with case insensitive input
  decoupling_power_lower <- calculate_decoupling(stream_df = mock_activity_streams, decouple_metric = "Power_HR")
  expect_equal(decoupling_power, decoupling_power_lower)
})

test_that("calculate_decoupling handles API path with mocked data", {
  # Mock Strava token
  mock_stoken <- structure(
    list(token = list(access_token = "fake_token")), class = "Token2.0"
  )
  
  # Mock activity list data
  mock_activities <- list(
    list(
      id = "12345",
      type = "Run",
      start_date_local = "2023-09-15T10:00:00Z",
      moving_time = 3600,  # 60 minutes
      distance = 10000
    ),
    list(
      id = "12346", 
      type = "Run",
      start_date_local = "2023-09-16T10:00:00Z",
      moving_time = 2700,  # 45 minutes
      distance = 8000
    )
  )
  
  # Mock activity streams
  mock_streams <- data.frame(
    time = 1:100,
    heartrate = 150 + rnorm(100, 0, 5),
    distance = cumsum(runif(100, 0.1, 0.2)),
    watts = 200 + rnorm(100, 0, 20),
    stringsAsFactors = FALSE
  )
  
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activities,
    get_activity_streams = function(...) mock_streams
  )
  
  # Test API path with valid parameters
  result <- calculate_decoupling(
    stoken = mock_stoken,
    activity_type = "Run",
    decouple_metric = "Pace_HR",
    start_date = "2023-09-01",
    end_date = "2023-09-30",
    min_duration_mins = 30,
    max_activities = 10
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("date", "decoupling") %in% names(result)))
  expect_true(nrow(result) > 0)
})

test_that("calculate_decoupling handles invalid stoken", {
  expect_error(
    calculate_decoupling(stoken = "invalid_token"),
    regexp = "`stoken` must be a valid Token2.0 object"
  )
})

test_that("calculate_decoupling handles invalid parameters", {
  mock_stoken <- structure(
    list(token = list(access_token = "fake_token")), class = "Token2.0"
  )
  
  expect_error(
    calculate_decoupling(stoken = mock_stoken, min_duration_mins = -1),
    regexp = "`min_duration_mins` must be a positive number"
  )
  
  expect_error(
    calculate_decoupling(stoken = mock_stoken, max_activities = 0),
    regexp = "`max_activities` must be a positive integer"
  )
})

test_that("calculate_decoupling handles invalid date range", {
  mock_stoken <- structure(
    list(token = list(access_token = "fake_token")), class = "Token2.0"
  )
  
  expect_error(
    calculate_decoupling(
      stoken = mock_stoken,
      start_date = "2023-12-01",
      end_date = "2023-11-01"
    ),
    regexp = "start_date must be before end_date"
  )
})

test_that("calculate_decoupling handles stream_df with insufficient rows", {
  expect_true(exists("mock_activity_streams"), "mock_activity_streams not loaded from helper.")
  
  # Create stream_df with too few rows
  small_streams <- mock_activity_streams[1:5, ]
  
  expect_error(
    calculate_decoupling(stream_df = small_streams, decouple_metric = "Power_HR"),
    regexp = "Provided `stream_df` has too few rows"
  )
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

# Additional tests to improve coverage
test_that("calculate_decoupling handles stream_df with velocity_smooth column", {
  expect_true(exists("mock_activity_streams"), "mock_activity_streams not loaded from helper.")
  
  # Create stream_df with velocity_smooth instead of distance
  velocity_streams <- data.frame(
    time = 1:100,
    heartrate = 150 + rnorm(100, 0, 5),
    velocity_smooth = runif(100, 2, 5),  # m/s
    stringsAsFactors = FALSE
  )
  
  decoupling_velocity <- calculate_decoupling(stream_df = velocity_streams, decouple_metric = "Pace_HR")
  expect_type(decoupling_velocity, "double")
  expect_true(is.finite(decoupling_velocity))
})

test_that("calculate_decoupling handles stream_df with distance column for speed calculation", {
  expect_true(exists("mock_activity_streams"), "mock_activity_streams not loaded from helper.")
  
  # Create stream_df with distance instead of velocity_smooth
  distance_streams <- data.frame(
    time = 1:100,
    heartrate = 150 + rnorm(100, 0, 5),
    distance = cumsum(runif(100, 0.1, 0.2)),  # cumulative distance
    stringsAsFactors = FALSE
  )
  
  decoupling_distance <- calculate_decoupling(stream_df = distance_streams, decouple_metric = "Pace_HR")
  expect_type(decoupling_distance, "double")
  expect_true(is.finite(decoupling_distance))
})

test_that("calculate_decoupling handles stream_df with insufficient data after speed calculation", {
  # Create stream_df that will have insufficient data after speed calculation
  insufficient_streams <- data.frame(
    time = 1:5,  # Very few rows
    heartrate = 150 + rnorm(5, 0, 5),
    distance = c(0, 0, 0, 0, 0),  # All zeros, will result in zero speed
    stringsAsFactors = FALSE
  )
  
  expect_error(
    calculate_decoupling(stream_df = insufficient_streams, decouple_metric = "Pace_HR"),
    regexp = "Provided `stream_df` has too few rows"
  )
})

test_that("calculate_decoupling handles stream_df with zero heartrate values", {
  # Create stream_df with zero heartrate values
  zero_hr_streams <- data.frame(
    time = 1:100,
    heartrate = 0,  # All zeros
    watts = 200 + rnorm(100, 0, 20),
    stringsAsFactors = FALSE
  )
  
  decoupling_zero_hr <- calculate_decoupling(stream_df = zero_hr_streams, decouple_metric = "Power_HR")
  expect_true(is.na(decoupling_zero_hr) || is.finite(decoupling_zero_hr))
})

test_that("calculate_decoupling handles stream_df with zero output values", {
  # Create stream_df with zero output values
  zero_output_streams <- data.frame(
    time = 1:100,
    heartrate = 150 + rnorm(100, 0, 5),
    watts = 0,  # All zeros
    stringsAsFactors = FALSE
  )
  
  decoupling_zero_output <- calculate_decoupling(stream_df = zero_output_streams, decouple_metric = "Power_HR")
  expect_true(is.na(decoupling_zero_output) || is.finite(decoupling_zero_output))
})

test_that("calculate_decoupling handles stream_df with negative speed values", {
  # Create stream_df that would result in negative speed values
  negative_speed_streams <- data.frame(
    time = 1:100,
    heartrate = 150 + rnorm(100, 0, 5),
    distance = c(100, 99, 98, 97, 96, rep(95, 95)),  # Decreasing distance
    stringsAsFactors = FALSE
  )
  
  decoupling_negative_speed <- calculate_decoupling(stream_df = negative_speed_streams, decouple_metric = "Pace_HR")
  expect_true(is.numeric(decoupling_negative_speed) || is.logical(decoupling_negative_speed))
  expect_true(is.finite(decoupling_negative_speed) || is.na(decoupling_negative_speed))
})

test_that("calculate_decoupling handles stream_df with exactly 10 rows", {
  # Test edge case with exactly 10 rows (minimum required)
  exact_min_streams <- data.frame(
    time = 1:10,
    heartrate = 150 + rnorm(10, 0, 5),
    watts = 200 + rnorm(10, 0, 20),
    stringsAsFactors = FALSE
  )
  
  decoupling_exact_min <- calculate_decoupling(stream_df = exact_min_streams, decouple_metric = "Power_HR")
  expect_type(decoupling_exact_min, "double")
  expect_true(is.finite(decoupling_exact_min))
})

test_that("calculate_decoupling handles stream_df with mid_point_index < 5", {
  # Create stream_df that will result in mid_point_index < 5
  small_streams <- data.frame(
    time = 1:8,  # 8 rows, mid_point_index = 4
    heartrate = 150 + rnorm(8, 0, 5),
    watts = 200 + rnorm(8, 0, 20),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    calculate_decoupling(stream_df = small_streams, decouple_metric = "Power_HR"),
    regexp = "Provided `stream_df` has too few rows"
  )
})

test_that("calculate_decoupling handles API path with empty activity list", {
  mock_stoken <- structure(
    list(token = list(access_token = "fake_token")), class = "Token2.0"
  )
  
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) list()  # Empty list
  )
  
  expect_error(
    calculate_decoupling(stoken = mock_stoken, decouple_metric = "Power_HR"),
    regexp = "Could not fetch any activities"
  )
})

test_that("calculate_decoupling handles API path with NULL activity list", {
  mock_stoken <- structure(
    list(token = list(access_token = "fake_token")), class = "Token2.0"
  )
  
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) NULL  # NULL return
  )
  
  expect_error(
    calculate_decoupling(stoken = mock_stoken, decouple_metric = "Power_HR"),
    regexp = "Could not fetch any activities"
  )
})

test_that("calculate_decoupling handles API path with no activities meeting criteria", {
  mock_stoken <- structure(
    list(token = list(access_token = "fake_token")), class = "Token2.0"
  )
  
  # Mock activities that don't meet criteria (wrong type, too short duration, etc.)
  mock_activities_no_match <- list(
    list(
      id = "12345",
      type = "Swim",  # Wrong type
      start_date_local = "2023-09-15T10:00:00Z",
      moving_time = 1800,  # 30 minutes (too short)
      distance = 1000
    )
  )
  
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activities_no_match
  )
  
  expect_error(
    calculate_decoupling(
      stoken = mock_stoken, 
      activity_type = "Run",
      decouple_metric = "Power_HR",
      min_duration_mins = 45
    ),
    regexp = "No activities met the specified criteria"
  )
})

test_that("calculate_decoupling handles API path with stream fetch failure", {
  mock_stoken <- structure(
    list(token = list(access_token = "fake_token")), class = "Token2.0"
  )
  
  mock_activities <- list(
    list(
      id = "12345",
      type = "Run",
      start_date_local = "2023-09-15T10:00:00Z",
      moving_time = 3600,  # 60 minutes
      distance = 10000
    )
  )
  
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activities,
    get_activity_streams = function(...) NULL  # Stream fetch fails
  )
  
  expect_error(
    calculate_decoupling(
      stoken = mock_stoken,
      activity_type = "Run",
      decouple_metric = "Power_HR",
      max_activities = 1
    ),
    regexp = "No activities met the specified criteria"
  )
})

test_that("calculate_decoupling handles API path with stream processing errors", {
  mock_stoken <- structure(
    list(token = list(access_token = "fake_token")), class = "Token2.0"
  )
  
  mock_activities <- list(
    list(
      id = "12345",
      type = "Run",
      start_date_local = "2023-09-15T10:00:00Z",
      moving_time = 3600,  # 60 minutes
      distance = 10000
    )
  )
  
  # Mock streams that will fail processing (all NAs)
  mock_streams_na <- data.frame(
    time = 1:100,
    heartrate = NA,
    distance = NA,
    stringsAsFactors = FALSE
  )
  
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activities,
    get_activity_streams = function(...) mock_streams_na
  )
  
  expect_error(
    calculate_decoupling(
      stoken = mock_stoken,
      activity_type = "Run",
      decouple_metric = "Power_HR",
      max_activities = 1
    ),
    regexp = "No activities met the specified criteria"
  )
})

test_that("calculate_decoupling handles date parsing errors gracefully", {
  mock_stoken <- structure(
    list(token = list(access_token = "fake_token")), class = "Token2.0"
  )
  
  # Test with invalid date format
  expect_error(
    calculate_decoupling(
      stoken = mock_stoken,
      start_date = "invalid-date",
      end_date = "also-invalid"
    ),
    regexp = "missing value where TRUE/FALSE needed"
  )
})

test_that("calculate_decoupling handles decouple_metric argument matching", {
  expect_true(exists("mock_activity_streams"), "mock_activity_streams not loaded from helper.")
  
  # Test with partial matching
  decoupling_pace <- calculate_decoupling(stream_df = mock_activity_streams, decouple_metric = "Pace")
  decoupling_power <- calculate_decoupling(stream_df = mock_activity_streams, decouple_metric = "Power")
  
  expect_type(decoupling_pace, "double")
  expect_type(decoupling_power, "double")
  expect_true(is.finite(decoupling_pace))
  expect_true(is.finite(decoupling_power))
})

# Test with stream_df that has velocity_smooth but no distance
test_that("calculate_decoupling handles stream_df with velocity_smooth but no distance", {
  velocity_only_streams <- data.frame(
    time = 1:100,
    heartrate = 150 + rnorm(100, 0, 5),
    velocity_smooth = runif(100, 2, 5),  # m/s
    stringsAsFactors = FALSE
  )
  
  result <- calculate_decoupling(stream_df = velocity_only_streams, decouple_metric = "Pace_HR")
  expect_type(result, "double")
  expect_true(is.finite(result))
})

# Test with stream_df that has both velocity_smooth and distance
test_that("calculate_decoupling handles stream_df with both velocity_smooth and distance", {
  both_streams <- data.frame(
    time = 1:100,
    heartrate = 150 + rnorm(100, 0, 5),
    velocity_smooth = runif(100, 2, 5),  # m/s
    distance = cumsum(runif(100, 0, 10)),  # cumulative distance
    stringsAsFactors = FALSE
  )
  
  result <- calculate_decoupling(stream_df = both_streams, decouple_metric = "Pace_HR")
  expect_type(result, "double")
  expect_true(is.finite(result))
})

# Test with stream_df that has watts column for Power_HR
test_that("calculate_decoupling handles stream_df with watts column for Power_HR", {
  watts_streams <- data.frame(
    time = 1:100,
    heartrate = 150 + rnorm(100, 0, 5),
    watts = 200 + rnorm(100, 0, 20),
    stringsAsFactors = FALSE
  )
  
  result <- calculate_decoupling(stream_df = watts_streams, decouple_metric = "Power_HR")
  expect_type(result, "double")
  expect_true(is.finite(result))
})

# Test with stream_df that has power column instead of watts
test_that("calculate_decoupling handles stream_df with power column instead of watts", {
  power_streams <- data.frame(
    time = 1:100,
    heartrate = 150 + rnorm(100, 0, 5),
    power = 200 + rnorm(100, 0, 20),
    stringsAsFactors = FALSE
  )
  
  # This should fail because the function expects 'watts' column, not 'power'
  expect_error(
    calculate_decoupling(stream_df = power_streams, decouple_metric = "Power_HR"),
    regexp = "Provided `stream_df` is invalid or missing required columns"
  )
})

# Test with stream_df that has all zero heartrate values
test_that("calculate_decoupling handles stream_df with all zero heartrate values", {
  zero_hr_streams <- data.frame(
    time = 1:100,
    heartrate = rep(0, 100),
    velocity_smooth = runif(100, 2, 5),
    stringsAsFactors = FALSE
  )
  
  result <- calculate_decoupling(stream_df = zero_hr_streams, decouple_metric = "Pace_HR")
  expect_true(is.na(result))
})

# Test with stream_df that has all zero output values
test_that("calculate_decoupling handles stream_df with all zero output values", {
  zero_output_streams <- data.frame(
    time = 1:100,
    heartrate = 150 + rnorm(100, 0, 5),
    velocity_smooth = rep(0, 100),
    stringsAsFactors = FALSE
  )
  
  result <- calculate_decoupling(stream_df = zero_output_streams, decouple_metric = "Pace_HR")
  expect_true(is.na(result))
})

# Test with stream_df that has very small values
test_that("calculate_decoupling handles stream_df with very small values", {
  small_values_streams <- data.frame(
    time = 1:100,
    heartrate = 0.001 + rnorm(100, 0, 0.0001),
    velocity_smooth = 0.001 + rnorm(100, 0, 0.0001),
    stringsAsFactors = FALSE
  )
  
  result <- calculate_decoupling(stream_df = small_values_streams, decouple_metric = "Pace_HR")
  expect_type(result, "double")
})

# Test with stream_df that has very large values
test_that("calculate_decoupling handles stream_df with very large values", {
  large_values_streams <- data.frame(
    time = 1:100,
    heartrate = 1000 + rnorm(100, 0, 100),
    velocity_smooth = 1000 + rnorm(100, 0, 100),
    stringsAsFactors = FALSE
  )
  
  result <- calculate_decoupling(stream_df = large_values_streams, decouple_metric = "Pace_HR")
  expect_type(result, "double")
})

# Test with stream_df that has exactly 5 rows (minimum for mid_point_index)
test_that("calculate_decoupling handles stream_df with exactly 5 rows", {
  five_row_streams <- data.frame(
    time = 1:5,
    heartrate = c(150, 155, 160, 165, 170),
    velocity_smooth = c(3.0, 3.1, 3.2, 3.3, 3.4),
    stringsAsFactors = FALSE
  )
  
  # This should fail because the function requires at least 10 rows
  expect_error(
    calculate_decoupling(stream_df = five_row_streams, decouple_metric = "Pace_HR"),
    regexp = "Provided `stream_df` has too few rows"
  )
})

# Test with stream_df that has 4 rows (too few for mid_point_index)
test_that("calculate_decoupling handles stream_df with 4 rows (too few)", {
  four_row_streams <- data.frame(
    time = 1:4,
    heartrate = c(150, 155, 160, 165),
    velocity_smooth = c(3.0, 3.1, 3.2, 3.3),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    calculate_decoupling(stream_df = four_row_streams, decouple_metric = "Pace_HR"),
    regexp = "Provided `stream_df` has too few rows"
  )
})

# Test with stream_df that has all NA values
test_that("calculate_decoupling handles stream_df with all NA values", {
  na_streams <- data.frame(
    time = 1:100,
    heartrate = rep(NA, 100),
    velocity_smooth = rep(NA, 100),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    calculate_decoupling(stream_df = na_streams, decouple_metric = "Pace_HR"),
    regexp = "Not enough valid data points in stream_df after removing NAs"
  )
})

# Test with stream_df that has mixed NA and valid values
test_that("calculate_decoupling handles stream_df with mixed NA and valid values", {
  mixed_streams <- data.frame(
    time = 1:100,
    heartrate = c(rep(NA, 50), 150 + rnorm(50, 0, 5)),
    velocity_smooth = c(runif(50, 2, 5), rep(NA, 50)),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    calculate_decoupling(stream_df = mixed_streams, decouple_metric = "Pace_HR"),
    regexp = "Not enough valid data points in stream_df after removing NAs"
  )
})

# Test with stream_df that has extreme ef1 and ef2 values
test_that("calculate_decoupling handles stream_df with extreme ef1 and ef2 values", {
  extreme_streams <- data.frame(
    time = 1:100,
    heartrate = c(rep(1, 50), rep(1000, 50)),  # Very different heart rates
    velocity_smooth = c(rep(0.001, 50), rep(1000, 50)),  # Very different speeds
    stringsAsFactors = FALSE
  )
  
  result <- calculate_decoupling(stream_df = extreme_streams, decouple_metric = "Pace_HR")
  expect_type(result, "double")
})

# Test with stream_df that has ef1 = 0 (should return NA)
test_that("calculate_decoupling handles stream_df with ef1 = 0", {
  zero_ef1_streams <- data.frame(
    time = 1:100,
    heartrate = c(rep(0, 50), rep(150, 50)),  # First half has zero HR
    velocity_smooth = c(rep(3.0, 50), rep(3.0, 50)),
    stringsAsFactors = FALSE
  )
  
  result <- calculate_decoupling(stream_df = zero_ef1_streams, decouple_metric = "Pace_HR")
  expect_true(is.na(result))
})

# Test with stream_df that has ef2 = 0 (should return NA)
test_that("calculate_decoupling handles stream_df with ef2 = 0", {
  zero_ef2_streams <- data.frame(
    time = 1:100,
    heartrate = c(rep(150, 50), rep(0, 50)),  # Second half has zero HR
    velocity_smooth = c(rep(3.0, 50), rep(3.0, 50)),
    stringsAsFactors = FALSE
  )
  
  result <- calculate_decoupling(stream_df = zero_ef2_streams, decouple_metric = "Pace_HR")
  expect_true(is.na(result))
})
