# tests/testthat/test-exposure.R

context("Load Exposure Calculation and Plotting")

# --- 测试 calculate_exposure (Skipped) ---
# NOTE: calculate_exposure tests skipped as they require mocking API calls.

# test_that("calculate_exposure output structure is correct", {
#   skip("Skipping calculate_exposure tests.")
# })
#
# test_that("calculate_exposure handles different load metrics", {
#   skip("Skipping calculate_exposure tests.")
# })

# --- 测试 plot_exposure (using mock data) ---

test_that("plot_exposure returns a ggplot object", {
  # Using mock_exposure_df from helper-mockdata.R
  expect_s3_class(plot_exposure(exposure_df = mock_exposure_df), "ggplot")
})

test_that("plot_exposure handles risk_zones argument", {
  expect_s3_class(plot_exposure(exposure_df = mock_exposure_df, risk_zones = TRUE), "ggplot")
  expect_s3_class(plot_exposure(exposure_df = mock_exposure_df, risk_zones = FALSE), "ggplot")
  
  # Check for abline layers (risk zones are drawn with ablines)
  p_zones <- plot_exposure(exposure_df = mock_exposure_df, risk_zones = TRUE)
  p_no_zones <- plot_exposure(exposure_df = mock_exposure_df, risk_zones = FALSE)
  get_abline_layers <- function(p) sum(sapply(p$layers, function(l) inherits(l$geom, "GeomAbline")))
  
  expect_gt(get_abline_layers(p_zones), 0)
  expect_equal(get_abline_layers(p_no_zones), 0)
})

test_that("plot_exposure handles empty data frame", {
    empty_df <- mock_exposure_df[0, ]
    expect_warning(p_empty <- plot_exposure(exposure_df = empty_df), "No valid exposure data available")
    expect_s3_class(p_empty, "ggplot")
    expect_equal(p_empty$labels$title, "No exposure data available")
}) 