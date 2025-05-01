# tests/testthat/test-ef.R

context("Efficiency Factor Calculation and Plotting")

# --- 测试 calculate_ef (Skipped) ---
# NOTE: calculate_ef tests are skipped. They likely require mocking API calls
# to provide mock_activity_list_df or refactoring to accept mock data.

# test_that("calculate_ef output structure is correct", {
#   skip("Skipping calculate_ef tests.")
# })
#
# test_that("calculate_ef handles different ef_metrics", {
#   skip("Skipping calculate_ef tests.")
# })
#
# test_that("calculate_ef handles missing data gracefully", {
#   skip("Skipping calculate_ef tests.")
# })
#
# test_that("calculate_ef applies min_duration_mins filter", {
#   skip("Skipping calculate_ef tests.")
# })

# --- 测试 plot_ef (using mock data) ---

test_that("plot_ef returns a ggplot object with mock data", {
  # Using mock_ef_df from helper-mockdata.R
  expect_s3_class(plot_ef(ef_df = mock_ef_df), "ggplot")
})

test_that("plot_ef handles add_trend_line argument", {
   expect_s3_class(plot_ef(ef_df = mock_ef_df, add_trend_line = TRUE), "ggplot")
   expect_s3_class(plot_ef(ef_df = mock_ef_df, add_trend_line = FALSE), "ggplot")
   
   # Check for geom_smooth layer presence/absence
   p_trend <- plot_ef(ef_df = mock_ef_df, add_trend_line = TRUE)
   p_no_trend <- plot_ef(ef_df = mock_ef_df, add_trend_line = FALSE)
   get_smooth_layers <- function(p) sum(sapply(p$layers, function(l) inherits(l$geom, "GeomSmooth")))
   expect_equal(get_smooth_layers(p_trend), 1)
   expect_equal(get_smooth_layers(p_no_trend), 0)
})

test_that("plot_ef handles empty data frame", {
    empty_df <- mock_ef_df[0, ]
    expect_warning(p_empty <- plot_ef(ef_df = empty_df), "No valid EF data available")
    expect_s3_class(p_empty, "ggplot")
    expect_equal(p_empty$labels$title, "No EF data available")
}) 