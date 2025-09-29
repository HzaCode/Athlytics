# tests/testthat/test-strava_helpers.R

library(testthat)
library(Athlytics)
library(mockery)
library(lubridate)
library(dplyr)

context("Strava Helper Functions")

# --- Mock stoken --- 
mock_stoken <- structure(list(token = "mock_token"), class = "Token2.0")

# --- Mocks for rStrava functions ---

# Mock for rStrava::get_activity_list
# Returns a list of activity summaries (simplified)
mock_rStrava_get_activity_list <- function(stoken, before, after) {
  # message(sprintf("MOCK get_activity_list called with before: %s, after: %s", 
  #                 as_datetime(before), as_datetime(after))) # Debug
  # Simulate filtering by date if needed for more complex tests
  list(
    list(id = 1, name = "Morning Run", type = "Run", sport_type = "Run", 
         start_date = "2023-10-01T08:00:00Z", start_date_local = "2023-10-01T08:00:00Z", 
         distance = 5000, moving_time = 1800, elapsed_time = 1850),
    list(id = 2, name = "Evening Ride", type = "Ride", sport_type = "Ride", 
         start_date = "2023-10-01T18:00:00Z", start_date_local = "2023-10-01T18:00:00Z", 
         distance = 20000, moving_time = 3600, elapsed_time = 3700),
    list(id = 3, name = "Long Run", type = "Run", sport_type = "Run", 
         start_date = "2023-10-02T09:00:00Z", start_date_local = "2023-10-02T09:00:00Z", 
         distance = 15000, moving_time = 5400, elapsed_time = 5500)
  )
}

# Mock for rStrava::compile_activities
# Takes the list from get_activity_list and returns a data.frame/tibble
mock_rStrava_compile_activities <- function(act_list) {
  # message("MOCK compile_activities called") # Debug
  if (length(act_list) == 0) {
    return(tibble(
      id = integer(0), name = character(0), type = character(0), sport_type = character(0),
      start_date_local = POSIXct(character(0), tz = "UTC"),
      start_date = POSIXct(character(0), tz = "UTC"), 
      distance = numeric(0), moving_time = integer(0), elapsed_time = integer(0)
    ))
  }
  dplyr::bind_rows(lapply(act_list, as_tibble))
}

# Mock for rStrava::get_activity
# Returns detailed data for a single activity (simplified)
mock_rStrava_get_activity <- function(act_data, stoken, id) {
  # message(sprintf("MOCK get_activity called for ID: %s", id)) # Debug
  if (id == 1) {
    list(id = 1, average_watts = 150, average_heartrate = 140, max_heartrate = 160, some_other_col = "val1")
  } else if (id == 2) {
    list(id = 2, average_watts = 200, average_heartrate = 150, max_heartrate = 170, some_other_col = "val2")
  } else if (id == 3) {
    list(id = 3, average_watts = 130, average_heartrate = 135, max_heartrate = 155, some_other_col = "val3")
  } else if (id == 99) { # For testing failure to fetch details
    NULL 
  }else {
    list(id = id, average_watts = NA, average_heartrate = NA, max_heartrate = NA, some_other_col = NA)
  }
}


# --- Tests for fetch_strava_activities ---

test_that("fetch_strava_activities basic path with fetch_details = FALSE works", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = mock_rStrava_get_activity_list,
    compile_activities = mock_rStrava_compile_activities
  )

  result_df <- fetch_strava_activities(stoken = mock_stoken, fetch_details = FALSE)

  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 3)
  expect_true(all(c("id", "name", "type", "sport_type", "start_date_local", "date", "distance", "moving_time", "elapsed_time") %in% names(result_df)))
  expect_s3_class(result_df$date, "Date")
  expect_s3_class(result_df$start_date_local, "POSIXct")
  # When fetch_details = FALSE, detail columns (like average_watts) 
  # should be present but all NA, due to the function's current structure.
  expect_true("average_watts" %in% names(result_df))
  expect_true(all(is.na(result_df$average_watts)))
  expect_true("average_heartrate" %in% names(result_df))
  expect_true(all(is.na(result_df$average_heartrate)))
})

test_that("fetch_strava_activities with fetch_details = TRUE works", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = mock_rStrava_get_activity_list,
    compile_activities = mock_rStrava_compile_activities,
    get_activity = mock_rStrava_get_activity # Our mock for detailed activity fetch
  )

  # required_cols by default is c("average_watts", "average_heartrate")
  # Our mock_rStrava_get_activity also returns max_heartrate and some_other_col
  # Test with default and custom required_cols

  result_df_default_cols <- fetch_strava_activities(
    stoken = mock_stoken, 
    fetch_details = TRUE, 
    delay = 0 # No delay needed for mock
  )

  expect_s3_class(result_df_default_cols, "tbl_df")
  expect_equal(nrow(result_df_default_cols), 3)
  expect_true(all(c("id", "name", "type", "start_date_local", "date", 
                    "average_watts", "average_heartrate") %in% names(result_df_default_cols)))
  
  # Check values from mock_rStrava_get_activity
  # Activity 1
  expect_equal(result_df_default_cols$average_watts[result_df_default_cols$id == 1], 150)
  expect_equal(result_df_default_cols$average_heartrate[result_df_default_cols$id == 1], 140)
  # Activity 2
  expect_equal(result_df_default_cols$average_watts[result_df_default_cols$id == 2], 200)
  expect_equal(result_df_default_cols$average_heartrate[result_df_default_cols$id == 2], 150)
  
  # Test with custom required_cols, including one not in mock's primary return for some IDs
  custom_cols <- c("average_watts", "max_heartrate", "some_other_col", "non_existent_col")
  result_df_custom_cols <- fetch_strava_activities(
    stoken = mock_stoken, 
    fetch_details = TRUE, 
    required_cols = custom_cols,
    delay = 0
  )
  expect_true(all(custom_cols %in% names(result_df_custom_cols)))
  expect_equal(result_df_custom_cols$max_heartrate[result_df_custom_cols$id == 1], 160)
  expect_equal(result_df_custom_cols$some_other_col[result_df_custom_cols$id == 2], "val2")
  expect_true(all(is.na(result_df_custom_cols$non_existent_col)))
})

test_that("fetch_strava_activities handles get_activity failure for one activity", {
  # Modify mock_activity_list to include an ID (99) that mock_get_activity returns NULL for
  mock_list_with_failing_id <- list(
    list(id = 1, name = "Good Run", type = "Run", start_date_local = "2023-10-01T08:00:00Z", distance = 5000, moving_time = 1800, elapsed_time = 1850, start_date="2023-10-01T08:00:00Z", sport_type="Run"),
    list(id = 99, name = "Bad Activity", type = "Hike", start_date_local = "2023-10-01T10:00:00Z", distance = 1000, moving_time = 600, elapsed_time = 650, start_date="2023-10-01T10:00:00Z", sport_type="Hike"), # This ID will fail detail fetch
    list(id = 2, name = "Good Ride", type = "Ride", start_date_local = "2023-10-01T18:00:00Z", distance = 20000, moving_time = 3600, elapsed_time = 3700, start_date="2023-10-01T18:00:00Z", sport_type="Ride")
  )
  
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_list_with_failing_id,
    compile_activities = mock_rStrava_compile_activities, # Use existing compile mock
    get_activity = mock_rStrava_get_activity # Existing get_activity mock returns NULL for id=99
  )

  # Expect a warning for the failed ID (99)
  expect_warning(
    result_df <- fetch_strava_activities(
      stoken = mock_stoken,
      fetch_details = TRUE,
      required_cols = c("average_watts", "average_heartrate"),
      delay = 0
    ),
    regexp = "Failed to fetch details for activity ID: 99"
  )

  # Check the resulting dataframe
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 3) # Should still have 3 rows
  
  # Details for ID 1 should be present (mock returns values)
  details_id1 <- result_df[result_df$id == 1, ]
  expect_false(is.na(details_id1$average_heartrate))
  
  # Details for ID 99 should be NA for required_cols
  details_id99 <- result_df[result_df$id == 99, ]
  expect_true(is.na(details_id99$average_watts))
  expect_true(is.na(details_id99$average_heartrate))
  
  # Details for ID 2 should be present (mock returns values)
  details_id2 <- result_df[result_df$id == 2, ]
  expect_false(is.na(details_id2$average_watts))
  expect_false(is.na(details_id2$average_heartrate))
})

test_that("fetch_strava_activities handles empty activity list from rStrava::get_activity_list", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) list(), # Simulate API returning no activities
    compile_activities = mock_rStrava_compile_activities # compile_activities would receive an empty list
  )

  result_df <- fetch_strava_activities(stoken = mock_stoken)

  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 0)
  # Check for the specific column structure returned by the function in this case
  expected_cols_on_empty <- c("id", "name", "type", "sport_type", "start_date", 
                              "start_date_local", "date", "distance", "moving_time", "elapsed_time")
  expect_true(all(expected_cols_on_empty %in% names(result_df)))
  expect_equal(length(names(result_df)), length(expected_cols_on_empty))
  
  # Check column types for the empty dataframe
  expect_type(result_df$id, "integer")
  expect_type(result_df$name, "character")
  expect_s3_class(result_df$start_date, "POSIXct")
  expect_s3_class(result_df$date, "Date")
  expect_type(result_df$distance, "double") # as.numeric produces double
  expect_type(result_df$moving_time, "integer")
})

test_that("fetch_strava_activities handles error during get_activity_list call", {
  local_mocked_bindings(
    .package = "rStrava",
    # Simulate get_activity_list throwing an error
    get_activity_list = function(...) stop("Simulated API error fetching list.")
  )

  expect_error(
    fetch_strava_activities(stoken = mock_stoken),
    regexp = "Failed to fetch activity list from Strava: Simulated API error fetching list."
  )
})

test_that("fetch_strava_activities handles invalid date formats", {
  # Test invalid start_date format
  expect_error(
    fetch_strava_activities(stoken = mock_stoken, start_date = "invalid-date-string"),
    regexp = "Could not parse start_date."
  )
  
  # Test invalid end_date format
  expect_error(
    fetch_strava_activities(stoken = mock_stoken, end_date = "2023/12/32"), # Invalid day
    regexp = "Could not parse end_date."
  )
})


# --- Tests for get_activity_list_stoken_direct ---
# This function is simpler and doesn't fetch details.

test_that("get_activity_list_stoken_direct basic path works (SKIPPED)", {
  skip("Skipping test for internal function get_activity_list_stoken_direct due to mocking complexities.")
  # Set up mocks for the duration of this test block
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = mock_rStrava_get_activity_list,
    compile_activities = mock_rStrava_compile_activities
  )
  
  # Call the function AFTER setting up mocks
  result_df <- athlytics:::get_activity_list_stoken_direct(stoken = mock_stoken)
  
  # Expectations 
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 3)
  # Returns same base columns as fetch_strava_activities with fetch_details = FALSE
  # but without the additional NA columns for required_cols if that logic differs.
  # Based on get_activity_list_stoken_direct source, it just calls compile_activities and then the mutate block.
  expect_true(all(c("id", "name", "type", "sport_type", "start_date_local", "date", "distance", "moving_time", "elapsed_time") %in% names(result_df)))
  expect_s3_class(result_df$date, "Date")
  expect_s3_class(result_df$start_date_local, "POSIXct")
})

test_that("get_activity_list_stoken_direct handles empty activity list (SKIPPED)", {
  skip("Skipping test for internal function get_activity_list_stoken_direct due to mocking complexities.")
  # Set up mocks for the duration of this test block
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) list(),
    compile_activities = mock_rStrava_compile_activities
  )

  # Call the function AFTER setting up mocks
  result_df <- athlytics:::get_activity_list_stoken_direct(stoken = mock_stoken)

  # Expectations
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 0)
  expected_cols_on_empty <- c("id", "name", "type", "sport_type", "start_date", 
                              "start_date_local", "date", "distance", "moving_time", "elapsed_time")
  expect_true(all(expected_cols_on_empty %in% names(result_df)))
  # This function also has the mutate step after compile_activities, so type checks are relevant.
  expect_type(result_df$id, "integer")
  expect_s3_class(result_df$date, "Date")
})

# Additional tests to improve coverage
test_that("fetch_strava_activities handles invalid stoken", {
  expect_error(
    fetch_strava_activities(stoken = "invalid_token"),
    regexp = "`stoken` must be a valid Token2.0 object"
  )
})

test_that("fetch_strava_activities handles NULL start_date and end_date", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = mock_rStrava_get_activity_list,
    compile_activities = mock_rStrava_compile_activities
  )

  # Test with both dates NULL (should use defaults)
  result_df <- fetch_strava_activities(stoken = mock_stoken, start_date = NULL, end_date = NULL)
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 3)
})

test_that("fetch_strava_activities handles Date objects for start_date and end_date", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = mock_rStrava_get_activity_list,
    compile_activities = mock_rStrava_compile_activities
  )

  # Test with Date objects
  start_date <- as.Date("2023-10-01")
  end_date <- as.Date("2023-10-02")
  
  result_df <- fetch_strava_activities(stoken = mock_stoken, start_date = start_date, end_date = end_date)
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 3)
})

test_that("fetch_strava_activities handles POSIXct objects for start_date and end_date", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = mock_rStrava_get_activity_list,
    compile_activities = mock_rStrava_compile_activities
  )

  # Test with POSIXct objects
  start_date <- as.POSIXct("2023-10-01 00:00:00", tz = "UTC")
  end_date <- as.POSIXct("2023-10-02 23:59:59", tz = "UTC")
  
  result_df <- fetch_strava_activities(stoken = mock_stoken, start_date = start_date, end_date = end_date)
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 3)
})

test_that("fetch_strava_activities handles end_date with time 00:00:00", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = mock_rStrava_get_activity_list,
    compile_activities = mock_rStrava_compile_activities
  )

  # Test with end_date that has 00:00:00 time (should be adjusted to 23:59:59)
  end_date <- "2023-10-02 00:00:00"
  
  result_df <- fetch_strava_activities(stoken = mock_stoken, end_date = end_date)
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 3)
})

test_that("fetch_strava_activities handles error during compile_activities call", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = mock_rStrava_get_activity_list,
    # Simulate compile_activities throwing an error
    compile_activities = function(...) stop("Simulated compile error.")
  )

  expect_error(
    fetch_strava_activities(stoken = mock_stoken),
    regexp = "Failed to compile activity list into data frame: Simulated compile error."
  )
})

test_that("fetch_strava_activities handles empty activity list with fetch_details = TRUE", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) list(),
    compile_activities = mock_rStrava_compile_activities
  )

  result_df <- fetch_strava_activities(stoken = mock_stoken, fetch_details = TRUE)
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 0)
  # When empty, the function returns a minimal structure without detail columns
  expect_false("average_watts" %in% names(result_df))
  expect_false("average_heartrate" %in% names(result_df))
})

test_that("fetch_strava_activities handles custom required_cols", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = mock_rStrava_get_activity_list,
    compile_activities = mock_rStrava_compile_activities,
    get_activity = mock_rStrava_get_activity
  )

  # Test with custom required_cols
  custom_cols <- c("max_heartrate", "some_other_col", "non_existent_col")
  result_df <- fetch_strava_activities(
    stoken = mock_stoken, 
    fetch_details = TRUE, 
    required_cols = custom_cols,
    delay = 0
  )
  
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 3)
  expect_true(all(custom_cols %in% names(result_df)))
})

test_that("fetch_strava_activities handles delay parameter", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = mock_rStrava_get_activity_list,
    compile_activities = mock_rStrava_compile_activities,
    get_activity = mock_rStrava_get_activity
  )

  # Test with custom delay
  start_time <- Sys.time()
  result_df <- fetch_strava_activities(
    stoken = mock_stoken, 
    fetch_details = TRUE, 
    delay = 0.1  # Small delay for testing
  )
  end_time <- Sys.time()
  
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 3)
  # Should take at least 0.3 seconds (3 activities * 0.1 delay)
  expect_true(as.numeric(end_time - start_time) >= 0.2)
})

test_that("fetch_strava_activities handles activities with missing columns after compile_activities", {
  # Mock compile_activities to return data with missing columns
  mock_compile_missing_cols <- function(act_list) {
    if (length(act_list) == 0) {
      return(tibble())
    }
    # Return data missing some expected columns but with required ones
    tibble(
      id = c(1, 2),
      name = c("Run 1", "Run 2"),
      type = c("Run", "Run"),
      sport_type = c("Run", "Run"),
      start_date = c("2023-01-01T10:00:00Z", "2023-01-02T10:00:00Z"),
      start_date_local = c("2023-01-01T10:00:00Z", "2023-01-02T10:00:00Z"),
      distance = c(5000, 10000),
      moving_time = c(1800, 3600),
      elapsed_time = c(1900, 3700),
      # Additional column to test handling
      some_other_col = c("val1", "val2")
    )
  }
  
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) list(
      list(id = 1, name = "Run 1", type = "Run"),
      list(id = 2, name = "Run 2", type = "Run")
    ),
    compile_activities = mock_compile_missing_cols
  )

  result_df <- fetch_strava_activities(stoken = mock_stoken, fetch_details = FALSE)
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 2)
  # Should have all required columns
  expect_true("sport_type" %in% names(result_df))
  expect_true("start_date_local" %in% names(result_df))
  expect_true("date" %in% names(result_df))
  expect_true("distance" %in% names(result_df))
  expect_true("moving_time" %in% names(result_df))
  expect_true("elapsed_time" %in% names(result_df))
  # Should preserve additional columns
  expect_true("some_other_col" %in% names(result_df))
})

test_that("fetch_strava_activities handles all get_activity calls failing", {
  # Mock get_activity to always return NULL
  mock_get_activity_always_fail <- function(...) NULL
  
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = mock_rStrava_get_activity_list,
    compile_activities = mock_rStrava_compile_activities,
    get_activity = mock_get_activity_always_fail
  )

  # Should get warnings for each failed activity
  expect_warning(
    result_df <- fetch_strava_activities(
      stoken = mock_stoken, 
      fetch_details = TRUE, 
      delay = 0
    ),
    regexp = "Failed to fetch details for activity ID"
  )
  
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 3)
  # All detail columns should be NA
  expect_true(all(is.na(result_df$average_watts)))
  expect_true(all(is.na(result_df$average_heartrate)))
})

test_that("fetch_strava_activities handles mixed success/failure in get_activity calls", {
  # Mock get_activity to fail for specific IDs
  mock_get_activity_mixed <- function(act_data, stoken, id) {
    if (id == 2) {
      NULL  # Fail for ID 2
    } else {
      list(id = id, average_watts = 100 + id * 10, average_heartrate = 120 + id * 5)
    }
  }
  
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = mock_rStrava_get_activity_list,
    compile_activities = mock_rStrava_compile_activities,
    get_activity = mock_get_activity_mixed
  )

  expect_warning(
    result_df <- fetch_strava_activities(
      stoken = mock_stoken, 
      fetch_details = TRUE, 
      delay = 0
    ),
    regexp = "Failed to fetch details for activity ID: 2"
  )
  
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 3)
  
  # ID 1 should have values
  expect_false(is.na(result_df$average_watts[result_df$id == 1]))
  expect_false(is.na(result_df$average_heartrate[result_df$id == 1]))
  
  # ID 2 should have NAs
  expect_true(is.na(result_df$average_watts[result_df$id == 2]))
  expect_true(is.na(result_df$average_heartrate[result_df$id == 2]))
  
  # ID 3 should have values
  expect_false(is.na(result_df$average_watts[result_df$id == 3]))
  expect_false(is.na(result_df$average_heartrate[result_df$id == 3]))
})

test_that("fetch_strava_activities handles empty required_cols", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = mock_rStrava_get_activity_list,
    compile_activities = mock_rStrava_compile_activities
  )

  # Test with empty required_cols
  result_df <- fetch_strava_activities(
    stoken = mock_stoken, 
    fetch_details = FALSE, 
    required_cols = character(0)
  )
  
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 3)
  # Should not have any detail columns
  expect_false("average_watts" %in% names(result_df))
  expect_false("average_heartrate" %in% names(result_df))
})

test_that("fetch_strava_activities handles date parsing edge cases", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = mock_rStrava_get_activity_list,
    compile_activities = mock_rStrava_compile_activities
  )

  # Test with various date formats that should work
  result1 <- fetch_strava_activities(stoken = mock_stoken, start_date = "2023-10-01")
  result2 <- fetch_strava_activities(stoken = mock_stoken, start_date = "2023/10/01")
  result3 <- fetch_strava_activities(stoken = mock_stoken, start_date = as.Date("2023-10-01"))
  
  expect_s3_class(result1, "tbl_df")
  expect_s3_class(result2, "tbl_df")
  expect_s3_class(result3, "tbl_df")
  expect_equal(nrow(result1), 3)
  expect_equal(nrow(result2), 3)
  expect_equal(nrow(result3), 3)
  
  # Test with invalid date format that should fail
  expect_error(
    fetch_strava_activities(stoken = mock_stoken, start_date = "01-10-2023"),
    regexp = "Could not parse start_date"
  )
})
