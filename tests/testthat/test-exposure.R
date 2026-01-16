# tests/testthat/test-exposure.R

library(testthat)
library(Athlytics)

# Load sample data from the package
data(sample_acwr)
data(sample_exposure)

# Load mock data (if helper-mockdata.R contains mocks for direct use)
source(test_path("helper-mockdata.R"), local = TRUE)

# Mock Strava token (if needed for functions that might call API, though most tests here use sample_df)

# --- Test plot_exposure (using pre-calculated ACWR data from sample data) ---

test_that("plot_exposure returns a ggplot object with sample_acwr data", {
  # Check if the sample ACWR data subset exists
  expect_true(exists("sample_acwr"), "sample_acwr not found.")
  expect_s3_class(sample_acwr, "data.frame")

  # Ensure sample_acwr has the expected columns for plotting
  # plot_exposure typically uses date, acwr, atl, ctl.
  expected_cols <- c("date", "acwr", "atl", "ctl")
  if (nrow(sample_acwr) > 0) { # Only check columns if data exists
    expect_true(
      all(expected_cols %in% names(sample_acwr)),
      paste("sample_acwr is missing one or more expected columns:", paste(expected_cols, collapse = ", "))
    )
  }

  # Test with actual data if it's not empty
  if (nrow(sample_acwr) > 0) {
    expect_s3_class(plot_exposure(exposure_df = sample_acwr), "ggplot")
  } else {
    skip("sample_acwr is empty, skipping main plot test.")
  }
})

test_that("plot_exposure handles risk_zones argument with sample_acwr", {
  if (!exists("sample_acwr") || nrow(sample_acwr) == 0) {
    skip("sample_acwr is empty or not found, skipping risk_zones test.")
  }
  expect_s3_class(plot_exposure(exposure_df = sample_acwr, risk_zones = TRUE), "ggplot")
  expect_s3_class(plot_exposure(exposure_df = sample_acwr, risk_zones = FALSE), "ggplot")

  # Check for geom_rect layers (risk zones are often drawn with geom_rect for background bands)
  # or geom_hline/geom_vline if specific lines are used.
  # The original test checked for GeomAbline, let's adapt if needed or keep if appropriate.
  # plot_exposure uses geom_abline for risk zones.
  p_zones <- plot_exposure(exposure_df = sample_acwr, risk_zones = TRUE)
  p_no_zones <- plot_exposure(exposure_df = sample_acwr, risk_zones = FALSE)

  get_abline_layers <- function(p) sum(sapply(p$layers, function(l) inherits(l$geom, "GeomAbline")))

  expect_equal(get_abline_layers(p_zones), 3)
  expect_equal(get_abline_layers(p_no_zones), 0)
})

test_that("plot_exposure handles empty data frame input", {
  # Create an empty data frame with the same structure as sample_acwr
  empty_df_structure <- data.frame(
    date = as.Date(character()),
    daily_load = numeric(),
    atl = numeric(),
    ctl = numeric(),
    acwr = numeric(),
    stringsAsFactors = FALSE
  )

  if (exists("sample_acwr") && nrow(sample_acwr) > 0) {
    empty_df <- sample_acwr[0, ]
  } else {
    empty_df <- empty_df_structure
  }

  expect_warning(
    p_empty <- plot_exposure(exposure_df = empty_df),
    regexp = "No valid exposure data available to plot \\(or missing required columns\\)."
  )
  expect_s3_class(p_empty, "ggplot")
  expect_true(grepl("No exposure data available", p_empty$labels$title, ignore.case = TRUE) || length(p_empty$layers) == 0)
})
