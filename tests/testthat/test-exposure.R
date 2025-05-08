# tests/testthat/test-exposure.R

library(testthat)
library(Athlytics)

# Load sample data from the package
data(Athlytics_sample_data)

# --- Test plot_exposure (using pre-calculated ACWR data from Athlytics_sample_data) ---

test_that("plot_exposure returns a ggplot object with athlytics_sample_acwr data", {
  # Check if the sample ACWR data subset exists
  expect_true(exists("athlytics_sample_acwr"), "athlytics_sample_acwr not found in Athlytics_sample_data.")
  expect_s3_class(athlytics_sample_acwr, "data.frame")
  
  # Ensure athlytics_sample_acwr has the expected columns for plotting
  # plot_exposure typically uses date, acwr, atl, ctl.
  expected_cols <- c("date", "acwr", "atl", "ctl")
  if (nrow(athlytics_sample_acwr) > 0) { # Only check columns if data exists
    expect_true(all(expected_cols %in% names(athlytics_sample_acwr)), 
                paste("athlytics_sample_acwr is missing one or more expected columns:", paste(expected_cols, collapse=", ")))
  }

  # Test with actual data if it's not empty
  if (nrow(athlytics_sample_acwr) > 0) {
      expect_s3_class(plot_exposure(exposure_df = athlytics_sample_acwr), "ggplot")
  } else {
      skip("athlytics_sample_acwr is empty, skipping main plot test.")
  }
})

test_that("plot_exposure handles risk_zones argument with athlytics_sample_acwr", {
  if (!exists("athlytics_sample_acwr") || nrow(athlytics_sample_acwr) == 0) {
    skip("athlytics_sample_acwr is empty or not found, skipping risk_zones test.")
  }
  expect_s3_class(plot_exposure(exposure_df = athlytics_sample_acwr, risk_zones = TRUE), "ggplot")
  expect_s3_class(plot_exposure(exposure_df = athlytics_sample_acwr, risk_zones = FALSE), "ggplot")
  
  # Check for geom_rect layers (risk zones are often drawn with geom_rect for background bands)
  # or geom_hline/geom_vline if specific lines are used.
  # The original test checked for GeomAbline, let's adapt if needed or keep if appropriate.
  # plot_exposure uses geom_abline for risk zones.
  p_zones <- plot_exposure(exposure_df = athlytics_sample_acwr, risk_zones = TRUE)
  p_no_zones <- plot_exposure(exposure_df = athlytics_sample_acwr, risk_zones = FALSE)
  
  get_abline_layers <- function(p) sum(sapply(p$layers, function(l) inherits(l$geom, "GeomAbline")))
  
  expect_equal(get_abline_layers(p_zones), 3)
  expect_equal(get_abline_layers(p_no_zones), 0)
})

test_that("plot_exposure handles empty data frame input", {
  # Create an empty data frame with the same structure as athlytics_sample_acwr
  empty_df_structure <- data.frame(
    date = as.Date(character()),
    daily_load = numeric(),
    atl = numeric(),
    ctl = numeric(),
    acwr = numeric(),
    stringsAsFactors = FALSE
  )
  
  if (exists("athlytics_sample_acwr") && nrow(athlytics_sample_acwr) > 0) {
      empty_df <- athlytics_sample_acwr[0, ]
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