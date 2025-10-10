# tests/testthat/test-pbs.R

library(testthat)
library(Athlytics)
library(ggplot2) # Explicitly load for s3_class checks if not automatically available
library(lubridate) # For seconds_to_period if used in manual_df

# Load data: sample data from package & mock API returns from helper
data(athlytics_sample_pbs)
source(test_path("helper-mockdata.R"), local = TRUE)

# NOTE: calculate_pbs tests are currently skipped because they require
# either mocking rStrava API calls or significant refactoring of the function
# to accept mock data directly.

# test_that("calculate_pbs output structure is correct (basic check)", {
#   skip("Skipping calculate_pbs tests until mocking/refactoring is done.")
#   # Example with mocking (using mockery or testthat::with_mock)
#   # mock_activity_list_data <- ...
#   # mock_detailed_activity_data <- ...
#   # with_mock(`rStrava::get_activity_list` = function(...) mock_activity_list_data,
#   #           `rStrava::get_activity` = function(...) mock_detailed_activity_data,
#   #           {
#   #             pbs_result <- calculate_pbs(stoken = "fake_token", activity_type = "Run", distance_meters = c(5000, 10000))
#   #             expect_s3_class(pbs_result, "tbl_df")
#   #             expect_true(all(c("activity_id", "activity_date", "distance", "time_seconds", "is_pb") %in% names(pbs_result)))
#   #           })
# })
#
# test_that("calculate_pbs handles different distances", {
#   skip("Skipping calculate_pbs tests until mocking/refactoring is done.")
# })

# --- Test plot_pbs using manually created, well-structured pbs_df ---

test_that("plot_pbs returns a ggplot object with valid pbs_df input", {
  # 1. Manually create a small, representative pbs_df
  #    This data frame should have the exact column names and types
  #    that calculate_pbs would output and plot_pbs expects.
  manual_pbs_df <- data.frame(
    activity_date = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15", "2023-01-01", "2023-01-20")),
    time_seconds = c(1200, 1180, 1190, 300, 1170),      # Example times
    distance = c(5000, 5000, 5000, 1000, 5000),         # Example distances
    is_pb = c(TRUE, TRUE, FALSE, TRUE, TRUE),          # Example PB flags
    distance_label = factor(c("5k", "5k", "5k", "1k", "5k"), levels = c("1k", "5k")),
    activity_id = as.character(1:5),
    elapsed_time = c(1200, 1180, 1190, 300, 1170),
    moving_time = c(1200, 1180, 1190, 300, 1170),
    cumulative_pb_seconds = c(1200, 1180, 1180, 300, 1170),
    # time_period is derived by plot_pbs if not present, or by calculate_pbs
    # For a direct pbs_df input test, ensure it has what plot_pbs *uses* or can work without.
    # calculate_pbs does output time_period.
    stringsAsFactors = FALSE
  )
  # Add time_period for completeness matching calculate_pbs output
  manual_pbs_df$time_period <- lubridate::seconds_to_period(manual_pbs_df$time_seconds)


  # distance_meters argument should match distances in manual_pbs_df
  test_distance_meters <- unique(manual_pbs_df$distance)

  # 2. Call plot_pbs with this manually created data
  p <- plot_pbs(pbs_df = manual_pbs_df, distance_meters = test_distance_meters)
  
  # 3. Perform assertions on the plot object 'p'
  expect_s3_class(p, "ggplot")
  
  # Check for expected layers if possible (e.g., points, lines)
  expect_true(length(p$layers) >= 2) # Expect at least geom_line and geom_point
  
  # Check labels (example)
  expect_equal(p$labels$x, "Activity Date")
  expect_equal(p$labels$y, "Best Time (MM:SS)")
  expect_equal(p$labels$title, "Personal Best Running Times Trend")

})

test_that("plot_pbs handles empty data frame input", {
  # Create an empty data frame with the correct column structure
  empty_df <- data.frame(
    activity_date = as.Date(character()),
    time_seconds = numeric(),
    distance = numeric(),
    is_pb = logical(),
    distance_label = factor(levels = c("1k", "5k")),
    activity_id = character(),
    elapsed_time = numeric(),
    moving_time = numeric(),
    cumulative_pb_seconds = numeric(),
    time_period = lubridate::seconds_to_period(numeric()),
    stringsAsFactors = FALSE
  )
  
  # plot_pbs requires distance_meters to be specified, even if pbs_df is empty,
  # because it uses it for filtering and potentially for calculate_pbs call.
  # However, if pbs_df is empty, the internal calculate_pbs won't be called.
  # The warning comes from `pbs_df[pbs_df$distance %in% distance_meters,]` if it results in empty or 
  # directly from the `nrow(pbs_df) == 0` check.
  expect_warning(
    p_empty <- plot_pbs(pbs_df = empty_df, distance_meters = c(1000, 5000)), 
    regexp = "No PB data available|pbs_df does not contain data for the specified distance_meters"
  )
  expect_s3_class(p_empty, "ggplot") 
  expect_true(grepl("No PB data available|No PB data for specified distances", p_empty$labels$title, ignore.case = TRUE))
})

# Test for add_trend_line argument
test_that("plot_pbs handles add_trend_line argument", {
    manual_pbs_df <- data.frame(
    activity_date = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15")), 
    time_seconds = c(1200, 1180, 1190), distance = c(5000, 5000, 5000),
    is_pb = c(TRUE, TRUE, FALSE), distance_label = factor(c("5k", "5k", "5k")), 
    activity_id = as.character(1:3), elapsed_time = c(1200,1180,1190),
    moving_time = c(1200,1180,1190), cumulative_pb_seconds = c(1200,1180,1180),
    stringsAsFactors = FALSE
  )
  manual_pbs_df$time_period <- lubridate::seconds_to_period(manual_pbs_df$time_seconds)
  test_dist_meters <- unique(manual_pbs_df$distance)

  p_trend <- plot_pbs(pbs_df = manual_pbs_df, distance_meters = test_dist_meters, add_trend_line = TRUE)
  p_no_trend <- plot_pbs(pbs_df = manual_pbs_df, distance_meters = test_dist_meters, add_trend_line = FALSE)
  
  get_smooth_layers <- function(p) sum(sapply(p$layers, function(l) inherits(l$geom, "GeomSmooth")))
  expect_equal(get_smooth_layers(p_trend), 1)
  expect_equal(get_smooth_layers(p_no_trend), 0)
})

# Optional: Test faceting if multiple distances are present
test_that("plot_pbs facets for multiple distances", {
  manual_pbs_df_multi_dist <- data.frame(
    activity_date = as.Date(c("2023-01-01", "2023-01-08", "2023-01-01")), 
    time_seconds = c(1200, 1180, 300), distance = c(5000, 5000, 1000),
    is_pb = c(TRUE, TRUE, TRUE), distance_label = factor(c("5k", "5k", "1k"), levels=c("1k", "5k")), 
    activity_id = as.character(1:3), elapsed_time = c(1200,1180,300),
    moving_time = c(1200,1180,300), cumulative_pb_seconds = c(1200,1180,300),
    stringsAsFactors = FALSE
  )
  manual_pbs_df_multi_dist$time_period <- lubridate::seconds_to_period(manual_pbs_df_multi_dist$time_seconds)
  test_dist_meters_multi <- unique(manual_pbs_df_multi_dist$distance)

  p_multi <- plot_pbs(pbs_df = manual_pbs_df_multi_dist, distance_meters = test_dist_meters_multi)
  
  # Check if faceting is applied (presence of FacetWrap class in plot object)
  is_faceted <- inherits(p_multi$facet, "FacetWrap")
  expect_true(is_faceted)
})

# Test error handling for missing parameters
test_that("plot_pbs handles missing parameters correctly", {
  # Test missing stoken when pbs_df is not provided
  expect_error(
    plot_pbs(distance_meters = c(1000, 5000)),
    regexp = "Either 'stoken' or 'pbs_df' must be provided"
  )
  
  # Test missing distance_meters when pbs_df is not provided
  expect_error(
    plot_pbs(stoken = "fake_token"),
    regexp = "`distance_meters` must be provided when `pbs_df` is not"
  )
  
  # Test missing distance_meters when pbs_df is provided but distance_meters is missing
  manual_pbs_df <- data.frame(
    activity_date = as.Date("2023-01-01"), 
    time_seconds = 1200, distance = 5000,
    is_pb = TRUE, distance_label = factor("5k"), 
    activity_id = "1", elapsed_time = 1200,
    moving_time = 1200, cumulative_pb_seconds = 1200,
    stringsAsFactors = FALSE
  )
  manual_pbs_df$time_period <- lubridate::seconds_to_period(manual_pbs_df$time_seconds)
  
  # This should work without error since pbs_df is provided
  p_result <- plot_pbs(pbs_df = manual_pbs_df)
  expect_s3_class(p_result, "ggplot")
})

# Test filtering by distance_meters
test_that("plot_pbs filters pbs_df by distance_meters correctly", {
  manual_pbs_df <- data.frame(
    activity_date = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15")), 
    time_seconds = c(1200, 1180, 300), distance = c(5000, 5000, 1000),
    is_pb = c(TRUE, TRUE, TRUE), distance_label = factor(c("5k", "5k", "1k"), levels=c("1k", "5k")), 
    activity_id = as.character(1:3), elapsed_time = c(1200,1180,300),
    moving_time = c(1200,1180,300), cumulative_pb_seconds = c(1200,1180,300),
    stringsAsFactors = FALSE
  )
  manual_pbs_df$time_period <- lubridate::seconds_to_period(manual_pbs_df$time_seconds)
  
  # Test filtering to only 5k distances
  p_filtered <- plot_pbs(pbs_df = manual_pbs_df, distance_meters = c(5000))
  expect_s3_class(p_filtered, "ggplot")
  
  # Test filtering to non-existent distance
  p_no_match <- plot_pbs(pbs_df = manual_pbs_df, distance_meters = c(10000))
  expect_s3_class(p_no_match, "ggplot")
  expect_true(grepl("No PB data for specified distances", p_no_match$labels$title, ignore.case = TRUE))
})

# Test with sample data from package
test_that("plot_pbs works with athlytics_sample_pbs", {
  # Check if sample data exists and has required columns
  if (exists("athlytics_sample_pbs") && is.data.frame(athlytics_sample_pbs) && nrow(athlytics_sample_pbs) > 0) {
    # Ensure required columns exist
    required_cols <- c("activity_date", "time_seconds", "distance", "is_pb", "distance_label")
    if (all(required_cols %in% names(athlytics_sample_pbs))) {
      # Get unique distances from sample data
      sample_distances <- unique(athlytics_sample_pbs$distance)
      
      p_sample <- plot_pbs(pbs_df = athlytics_sample_pbs, distance_meters = sample_distances)
      expect_s3_class(p_sample, "ggplot")
    }
  } else {
    skip("athlytics_sample_pbs not available or empty")
  }
})

# test_that("plot_pbs works with multiple distances", { # Covered by the first test
#    # Use mock_pbs_df which already has multiple distances
#    expect_s3_class(plot_pbs(pbs_df = mock_pbs_df), "ggplot")
# }) 
