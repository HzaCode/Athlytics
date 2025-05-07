# tests/testthat/test-calculate_pbs.R

library(testthat)
library(Athlytics)
library(mockery)
# library(rStrava) # Not calling rStrava directly

# Load mock data helpers
data(Athlytics_sample_data) # Though not directly used here, good practice
source(test_path("helpe-mockdata.R"), local = TRUE) # For mock_activity_list_list

# Mock Strava token
mock_stoken <- structure(list(token = list(access_token = "fake_token")), class = "Token2.0")

# --- Test Parameter Validation ---

test_that("calculate_pbs throws error for non-Token2.0 stoken", {
  expect_error(
    calculate_pbs(stoken = data.frame(), distance_meters = 1000),
    # The error message might change with proper mocking if it gets past stoken check
    # For now, assume it still checks stoken type first or fails on a subsequent rStrava call expectation
    regexp = "Assertion on 'stoken' failed: Must inherit from class 'Token2.0', but has class 'data.frame'\\."
  )
})

test_that("calculate_pbs throws error for non-numeric distance_meters", {
  expect_error(
    calculate_pbs(stoken = mock_stoken, distance_meters = "1km"),
    regexp = "'distance_meters' must be a numeric vector of distances \\(e\\.g\\., c\\(1000, 5000\\)\\)\\."
  )
})

test_that("calculate_pbs throws error for zero or negative distance_meters", {
    expect_error(
        calculate_pbs(stoken = mock_stoken, distance_meters = c(1000, 0)),
        regexp = "All 'distance_meters' must be positive\\."
    )
    expect_error(
        calculate_pbs(stoken = mock_stoken, distance_meters = -1000),
        regexp = "All 'distance_meters' must be positive\\."
    )
})

# --- Test API Interaction and Core Logic with Mocking ---

# Helper to create a mock rStrava::get_efforts_list return value
# This function will simulate finding best efforts for given distances within a single activity's efforts list
# It's a simplified mock focusing on returning plausible data for calculate_pbs

# The actual get_efforts_list returns a list of data frames, one for each segment effort in an activity.
# Each data frame has columns like: id, resource_state, name, activity, athlete, elapsed_time, moving_time, start_date, start_date_local, distance, start_index, end_index, pr_rank, segment.id, segment.name, etc.
# For calculate_pbs, we are interested in finding the best `elapsed_time` for a `distance` that is close to the target distances.
# The function internally seems to look for `segment.distance` matching `distance_meters`.
# Let's make our mock simpler: assume get_efforts_list returns a list of efforts, and each effort is a list/df with at least 'distance' and 'elapsed_time'.

# For now, let's assume a mock that returns a list of efforts for *one* activity
# To test calculate_pbs properly, we need to simulate get_efforts_list being called for *each* activity from get_activity_list

# Mock for get_efforts_list: needs to be adaptable or we mock its caller if calculate_pbs has an internal helper.
# calculate_pbs directly calls get_efforts_list(act_data = activity_list, stoken = stoken, id = current_activity_id)
# So, the mock needs to handle different `id` arguments if we want to simulate different efforts for different activities.
# For simplicity, let's make it return the *same* set of efforts for any activity ID, but this means PBs will always come from the same mocked efforts.

mock_efforts_data_for_activity <- list(
  data.frame( # Effort 1
    name = "Sprint to Tree",
    distance = 100, # meters
    elapsed_time = 15, # seconds
    start_date = as.POSIXct("2023-10-01 10:00:00", tz = "UTC")
  ),
  data.frame( # Effort 2
    name = "400m Dash",
    distance = 400, # meters
    elapsed_time = 60, # seconds
    start_date = as.POSIXct("2023-10-01 10:05:00", tz = "UTC")
  ),
  data.frame( # Effort 3: close to 1k
    name = "Near Kilometre",
    distance = 990, # meters
    elapsed_time = 180, # 3 mins
    start_date = as.POSIXct("2023-10-01 10:10:00", tz = "UTC")
  ),
   data.frame( # Effort 4: another near 1k, slightly slower
    name = "Another Near Kilometre",
    distance = 995, # meters
    elapsed_time = 185, # 3 mins 5s
    start_date = as.POSIXct("2023-10-01 10:15:00", tz = "UTC")
  ),
  data.frame( # Effort 5: close to 5k
    name = "Long Stretch",
    distance = 4950, # meters
    elapsed_time = 1200, # 20 mins
    start_date = as.POSIXct("2023-10-01 10:20:00", tz = "UTC")
  )
)

test_that("calculate_pbs (API path) processes mocked data correctly", {
  
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list, # Returns a list of 2 activities
    get_efforts_list = function(act_data, stoken, id, ...) { 
      # message(sprintf("MOCK get_efforts_list called for activity ID: %s", id)) # Debug
      # Regardless of id, return the same mock efforts for simplicity in this test
      # In a more complex test, we could vary this based on `id` if mock_activity_list_list had known IDs
      return(mock_efforts_data_for_activity)
    }
  )

  target_distances <- c(1000, 5000) # Looking for PBs around 1km and 5km
  
  result_df <- calculate_pbs(
    stoken = mock_stoken,
    distance_meters = target_distances,
    activity_type = "Run", # Matches mock_activity_list_list types
    max_activities = 2 # Use all activities from mock_activity_list_list
  )
  
  expect_s3_class(result_df, "data.frame")
  expect_named(result_df, c("distance_target_m", "distance_actual_m", "best_time_seconds", "date", "activity_id", "activity_name"), ignore.order = TRUE)
  expect_equal(nrow(result_df), length(target_distances)) # One row per target distance
  
  # Check for 1000m PB (should pick 990m or 995m effort)
  pb_1k <- result_df[result_df$distance_target_m == 1000, ]
  expect_equal(pb_1k$best_time_seconds, 180) # From the 990m effort
  expect_true(pb_1k$distance_actual_m %in% c(990, 995))
  
  # Check for 5000m PB (should pick 4950m effort)
  pb_5k <- result_df[result_df$distance_target_m == 5000, ]
  expect_equal(pb_5k$best_time_seconds, 1200)
  expect_equal(pb_5k$distance_actual_m, 4950)
  
  # All activity IDs in results should be from mock_activity_list_list
  # This assumes mock_activity_list_list items have an 'id' field
  mock_activity_ids <- sapply(mock_activity_list_list, function(x) x$id)
  expect_true(all(result_df$activity_id %in% mock_activity_ids))
})

test_that("calculate_pbs (API path) handles no matching efforts found", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list,
    get_efforts_list = function(...) list() # Simulate no efforts found for any activity
  )
  
  result_df <- calculate_pbs(
    stoken = mock_stoken,
    distance_meters = c(100, 1000),
    max_activities = 1
  )
  
  expect_s3_class(result_df, "data.frame")
  # Expect NA for times if no efforts match, or fewer rows if distance_target_m is omitted
  # Based on current implementation, it returns NA for best_time_seconds and other details.
  expect_true(all(is.na(result_df$best_time_seconds)))
  expect_true(all(is.na(result_df$activity_id)))
  expect_equal(nrow(result_df), 2) # Still returns rows for each target distance
})

test_that("calculate_pbs (API path) handles empty activity list from mock", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) list() # Return empty list
  )
  expect_error(
    calculate_pbs(stoken = mock_stoken, distance_meters = 1000),
    regexp = "Could not fetch any activities\\.|No activities found matching the criteria\\."
  )
})

# Test for date_range filtering (if calculate_pbs implements it for get_activity_list)
# calculate_pbs passes start_date, end_date to get_activity_list_stoken_direct, 
# which passes them to rStrava::get_activity_list. So, mocking get_activity_list 
# means this filtering is implicitly part of what get_activity_list mock returns.
# A specific test would involve a mock_activity_list_list with varied dates and 
# checking if calculate_pbs respects the date window by only processing those.
# This is more a test of get_activity_list_stoken_direct or the mock of get_activity_list.

# The existing test for unsupported activity_type = "Ride" might pass if the mock for 
# get_activity_list returns activities that are not "Run", and then get_efforts_list
# returns empty or non-matching efforts. Or it might fail if it expects only "Run" activities.
# Let's update that test.
test_that("calculate_pbs handles activity_type filter correctly with mock", {
  # mock_activity_list_list contains both Runs and Rides
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list,
    get_efforts_list = function(...) mock_efforts_data_for_activity 
  )
  
  # Requesting "Hike" which is not in mock_activity_list_list
  # This should result in an error from calculate_pbs if no activities are found after filtering.
  expect_error(
    calculate_pbs(stoken = mock_stoken, distance_meters = 1000, activity_type = "Hike", max_activities = 5),
    regexp = "No activities found matching the criteria\\."
  )
  
  # Requesting "Ride", which IS in mock_activity_list_list
  # Should produce results if mock_efforts_data_for_activity are deemed suitable for rides
  # (the mock efforts are generic enough)
  result_ride_df <- calculate_pbs(
    stoken = mock_stoken, 
    distance_meters = c(100, 400), # Distances in mock_efforts_data_for_activity
    activity_type = "Ride", 
    max_activities = 5
  )
  expect_s3_class(result_ride_df, "data.frame")
  expect_true(nrow(result_ride_df) == 2) # Two distances requested
  # Check if times are populated (e.g., 15s for 100m, 60s for 400m from mock_efforts_data_for_activity)
  expect_equal(result_ride_df$best_time_seconds[result_ride_df$distance_target_m == 100], 15)
  expect_equal(result_ride_df$best_time_seconds[result_ride_df$distance_target_m == 400], 60)
}) 