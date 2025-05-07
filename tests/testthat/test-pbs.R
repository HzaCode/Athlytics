# tests/testthat/test-pbs.R

library(testthat)
library(Athlytics)

# Load sample data from the package
data(Athlytics_sample_data)

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

# --- Test plot_pbs (using pre-calculated data from Athlytics_sample_data) ---

test_that("plot_pbs returns a ggplot object with athlytics_sample_pbs data", {
  # Check if the sample data subset exists
  expect_true(exists("athlytics_sample_pbs"), "athlytics_sample_pbs not found in Athlytics_sample_data.")
  expect_s3_class(athlytics_sample_pbs, "data.frame")
  
  # Ensure pbs_df has the expected columns for plotting
  # calculate_pbs output includes: distance_target_m, best_time_seconds, date
  # plot_pbs expects these, possibly others if it transforms data.
  # Based on plot_pbs source, it expects at least: date, best_time_seconds, distance_target_m
  expected_cols <- c("date", "best_time_seconds", "distance_target_m")
  if (nrow(athlytics_sample_pbs) > 0) { # Only check columns if data exists
    expect_true(all(expected_cols %in% names(athlytics_sample_pbs)), 
                paste("athlytics_sample_pbs is missing one or more expected columns:", paste(expected_cols, collapse=", ")))
  }

  # Test with actual data if it's not empty
  if (nrow(athlytics_sample_pbs) > 0) {
      expect_s3_class(plot_pbs(pbs_df = athlytics_sample_pbs), "ggplot")
  } else {
      # If athlytics_sample_pbs is empty, test the empty case handling
      # This scenario is also covered by the next test explicitly
      # but good to acknowledge if sample data might be empty.
      # For now, assume athlytics_sample_pbs should ideally have data for this test.
      # If it must be tested with potentially empty sample data, the warning/empty plot checks apply.
      skip("athlytics_sample_pbs is empty, skipping main plot test.")
  }
})

test_that("plot_pbs handles empty data frame input", {
  # Create an empty data frame with the same structure as athlytics_sample_pbs
  # Ensure all necessary columns for plot_pbs are present even if empty
  empty_df_structure <- data.frame(
    date = as.Date(character()),
    best_time_seconds = numeric(),
    distance_target_m = numeric(),
    activity_id = character(), # Add other columns if plot_pbs uses them
    activity_name = character(),
    distance_actual_m = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Ensure the structure matches athlytics_sample_pbs if it's not empty, 
  # otherwise use the defined structure.
  if (exists("athlytics_sample_pbs") && nrow(athlytics_sample_pbs) > 0) {
      empty_df <- athlytics_sample_pbs[0, ]
  } else {
      empty_df <- empty_df_structure
  }
  
  expect_warning(
    p_empty <- plot_pbs(pbs_df = empty_df), 
    regexp = "No PB data available to plot\\.|Input data frame is empty\\."
  )
  expect_s3_class(p_empty, "ggplot") 
  # Check if the plot title indicates no data or is a base empty ggplot
  expect_true(grepl("No PB data available", p_empty$labels$title, ignore.case = TRUE) || length(p_empty$layers) == 0)
})

# test_that("plot_pbs works with multiple distances", { # Covered by the first test
#    # Use mock_pbs_df which already has multiple distances
#    expect_s3_class(plot_pbs(pbs_df = mock_pbs_df), "ggplot")
# }) 