# tests/testthat/test-pbs.R

context("Personal Bests Calculation and Plotting")

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

# --- 测试 plot_pbs (using mock data) ---

test_that("plot_pbs returns a ggplot object with mock data", {
  # 使用 helper 文件中定义的 mock_pbs_df
  expect_s3_class(plot_pbs(pbs_df = mock_pbs_df), "ggplot")
})

test_that("plot_pbs handles empty data frame", {
  empty_df <- mock_pbs_df[0, ] # Create an empty df with same columns
  # Expect a warning and potentially a specific type of empty plot
  expect_warning(p_empty <- plot_pbs(pbs_df = empty_df), "No PB data available to plot")
  expect_s3_class(p_empty, "ggplot") # Should still return a ggplot object
  # Optionally check if it's the specific empty plot structure we defined
  expect_true(length(p_empty$layers) == 0) # Check if it has no data layers
  expect_equal(p_empty$labels$title, "No PB data available")
})

# test_that("plot_pbs works with multiple distances", { # Covered by the first test
#    # Use mock_pbs_df which already has multiple distances
#    expect_s3_class(plot_pbs(pbs_df = mock_pbs_df), "ggplot")
# }) 