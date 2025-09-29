# tests/testthat/test-exposure.R

library(testthat)
library(Athlytics)

# Load sample data from the package
data(athlytics_sample_data)

# Load mock data (if helper-mockdata.R contains mocks for direct use)
source(test_path("helper-mockdata.R"), local = TRUE)

# Mock Strava token (if needed for functions that might call API, though most tests here use sample_df)

# --- Test plot_exposure (using pre-calculated ACWR data from athlytics_sample_data) ---

test_that("plot_exposure returns a ggplot object with athlytics_sample_acwr data", {
  # Check if the sample ACWR data subset exists
  expect_true(exists("athlytics_sample_acwr"), "athlytics_sample_acwr not found in athlytics_sample_data.")
  expect_s3_class(athlytics_sample_acwr, "data.frame")
  
  # Check athlytics_sample_acwr has expected columns for plotting
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

test_that("plot_exposure handles missing parameters correctly", {
  # Test missing stoken when exposure_df is not provided
  expect_error(
    plot_exposure(activity_type = "Run", load_metric = "duration_mins"),
    regexp = "Either 'stoken' or 'exposure_df' must be provided"
  )
})

test_that("plot_exposure handles invalid exposure_df", {
  # Test with empty data frame
  empty_df <- data.frame()
  expect_warning(
    p_empty <- plot_exposure(exposure_df = empty_df),
    regexp = "No valid exposure data available to plot"
  )
  expect_s3_class(p_empty, "ggplot")
  expect_true(grepl("No exposure data available", p_empty$labels$title, ignore.case = TRUE))
  
  # Test with data frame missing required columns
  df_missing_cols <- data.frame(date = Sys.Date(), atl = 10)
  expect_warning(
    p_missing <- plot_exposure(exposure_df = df_missing_cols),
    regexp = "No valid exposure data available to plot"
  )
  expect_s3_class(p_missing, "ggplot")
})

test_that("plot_exposure works with different load_metric options", {
  # Create sample data with different load metrics
  sample_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    atl = c(10, 12, 11),
    ctl = c(8, 9, 10),
    acwr = c(1.25, 1.33, 1.1),
    stringsAsFactors = FALSE
  )
  
  # Test with different load metrics
  p_duration <- plot_exposure(exposure_df = sample_data, load_metric = "duration_mins")
  expect_s3_class(p_duration, "ggplot")
  
  p_distance <- plot_exposure(exposure_df = sample_data, load_metric = "distance_km")
  expect_s3_class(p_distance, "ggplot")
  
  p_tss <- plot_exposure(exposure_df = sample_data, load_metric = "tss")
  expect_s3_class(p_tss, "ggplot")
  
  p_hrss <- plot_exposure(exposure_df = sample_data, load_metric = "hrss")
  expect_s3_class(p_hrss, "ggplot")
})

test_that("plot_exposure handles risk_zones with and without ACWR column", {
  # Test with ACWR column
  sample_data_with_acwr <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    atl = c(10, 12, 11),
    ctl = c(8, 9, 10),
    acwr = c(1.25, 1.33, 1.1),
    stringsAsFactors = FALSE
  )
  
  p_with_zones <- plot_exposure(exposure_df = sample_data_with_acwr, risk_zones = TRUE)
  expect_s3_class(p_with_zones, "ggplot")
  
  # Test without ACWR column
  sample_data_no_acwr <- sample_data_with_acwr[, !names(sample_data_with_acwr) %in% "acwr"]
  expect_warning(
    p_no_acwr <- plot_exposure(exposure_df = sample_data_no_acwr, risk_zones = TRUE),
    regexp = "ACWR column not found"
  )
  expect_s3_class(p_no_acwr, "ggplot")
})

test_that("plot_exposure handles different acute_period and chronic_period", {
  sample_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    atl = c(10, 12, 11),
    ctl = c(8, 9, 10),
    acwr = c(1.25, 1.33, 1.1),
    stringsAsFactors = FALSE
  )
  
  p_custom_periods <- plot_exposure(
    exposure_df = sample_data,
    acute_period = 14,
    chronic_period = 28
  )
  expect_s3_class(p_custom_periods, "ggplot")
  
  # Check that the subtitle contains the custom periods
  expect_true(grepl("Acute: 14 days", p_custom_periods$labels$subtitle))
  expect_true(grepl("Chronic: 28 days", p_custom_periods$labels$subtitle))
})

# Additional tests to improve coverage
test_that("plot_exposure handles elevation_gain_m load_metric", {
  sample_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    atl = c(10, 12, 11),
    ctl = c(8, 9, 10),
    acwr = c(1.25, 1.33, 1.1),
    stringsAsFactors = FALSE
  )
  
  p_elevation <- plot_exposure(exposure_df = sample_data, load_metric = "elevation_gain_m")
  expect_s3_class(p_elevation, "ggplot")
  
  # Check that the title contains the correct metric label
  expect_true(grepl("Elevation Gain \\(m\\)", p_elevation$labels$title))
})

test_that("plot_exposure handles custom load_metric", {
  sample_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    atl = c(10, 12, 11),
    ctl = c(8, 9, 10),
    acwr = c(1.25, 1.33, 1.1),
    stringsAsFactors = FALSE
  )
  
  p_custom <- plot_exposure(exposure_df = sample_data, load_metric = "custom_metric")
  expect_s3_class(p_custom, "ggplot")
  
  # Check that the title contains the custom metric name
  expect_true(grepl("custom_metric", p_custom$labels$title))
})

test_that("plot_exposure handles single data point", {
  single_point_data <- data.frame(
    date = as.Date("2023-01-01"),
    atl = 10,
    ctl = 8,
    acwr = 1.25,
    stringsAsFactors = FALSE
  )
  
  p_single <- plot_exposure(exposure_df = single_point_data, risk_zones = TRUE)
  expect_s3_class(p_single, "ggplot")
  expect_equal(nrow(p_single$data), 1)
})

test_that("plot_exposure handles data with zero values", {
  zero_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02")),
    atl = c(0, 0),
    ctl = c(0, 0),
    acwr = c(0, 0),
    stringsAsFactors = FALSE
  )
  
  p_zero <- plot_exposure(exposure_df = zero_data, risk_zones = TRUE)
  expect_s3_class(p_zero, "ggplot")
})

test_that("plot_exposure handles data with NA values", {
  na_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    atl = c(10, NA, 11),
    ctl = c(8, 9, NA),
    acwr = c(1.25, 1.33, 1.1),
    stringsAsFactors = FALSE
  )
  
  p_na <- plot_exposure(exposure_df = na_data, risk_zones = TRUE)
  expect_s3_class(p_na, "ggplot")
})

test_that("plot_exposure handles data with very large values", {
  large_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02")),
    atl = c(1000, 1200),
    ctl = c(800, 900),
    acwr = c(1.25, 1.33),
    stringsAsFactors = FALSE
  )
  
  p_large <- plot_exposure(exposure_df = large_data, risk_zones = TRUE)
  expect_s3_class(p_large, "ggplot")
})

test_that("plot_exposure handles data with negative values", {
  negative_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02")),
    atl = c(-10, -12),
    ctl = c(-8, -9),
    acwr = c(1.25, 1.33),
    stringsAsFactors = FALSE
  )
  
  p_negative <- plot_exposure(exposure_df = negative_data, risk_zones = TRUE)
  expect_s3_class(p_negative, "ggplot")
})

test_that("plot_exposure handles risk zones with zero max values", {
  zero_max_data <- data.frame(
    date = as.Date("2023-01-01"),
    atl = 0,
    ctl = 0,
    acwr = 0,
    stringsAsFactors = FALSE
  )
  
  p_zero_max <- plot_exposure(exposure_df = zero_max_data, risk_zones = TRUE)
  expect_s3_class(p_zero_max, "ggplot")
})

test_that("plot_exposure handles risk zones with very small values", {
  small_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02")),
    atl = c(0.001, 0.002),
    ctl = c(0.001, 0.001),
    acwr = c(1.0, 2.0),
    stringsAsFactors = FALSE
  )
  
  p_small <- plot_exposure(exposure_df = small_data, risk_zones = TRUE)
  expect_s3_class(p_small, "ggplot")
})

test_that("plot_exposure handles multiple activity types", {
  sample_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    atl = c(10, 12, 11),
    ctl = c(8, 9, 10),
    acwr = c(1.25, 1.33, 1.1),
    stringsAsFactors = FALSE
  )
  
  p_multi <- plot_exposure(
    exposure_df = sample_data,
    activity_type = c("Run", "Ride", "VirtualRun", "VirtualRide")
  )
  expect_s3_class(p_multi, "ggplot")
})

test_that("plot_exposure handles different date ranges", {
  # Test with dates spanning multiple years
  multi_year_data <- data.frame(
    date = as.Date(c("2022-01-01", "2022-06-15", "2023-01-01", "2023-12-31")),
    atl = c(10, 12, 11, 15),
    ctl = c(8, 9, 10, 12),
    acwr = c(1.25, 1.33, 1.1, 1.25),
    stringsAsFactors = FALSE
  )
  
  p_multi_year <- plot_exposure(exposure_df = multi_year_data, risk_zones = TRUE)
  expect_s3_class(p_multi_year, "ggplot")
})

test_that("plot_exposure handles large datasets", {
  # Create a larger dataset
  large_dates <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "week")
  large_data <- data.frame(
    date = large_dates,
    atl = 10 + rnorm(length(large_dates), 0, 2),
    ctl = 8 + rnorm(length(large_dates), 0, 1),
    acwr = 1.2 + rnorm(length(large_dates), 0, 0.1),
    stringsAsFactors = FALSE
  )
  
  p_large <- plot_exposure(exposure_df = large_data, risk_zones = TRUE)
  expect_s3_class(p_large, "ggplot")
  expect_equal(nrow(p_large$data), nrow(large_data))
})

test_that("plot_exposure handles message output", {
  sample_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02")),
    atl = c(10, 12),
    ctl = c(8, 9),
    acwr = c(1.25, 1.33),
    stringsAsFactors = FALSE
  )
  
  expect_message(
    plot_exposure(exposure_df = sample_data),
    regexp = "Generating plot"
  )
})

test_that("plot_exposure handles NULL exposure_df", {
  expect_error(
    plot_exposure(exposure_df = NULL),
    regexp = "Either 'stoken' or 'exposure_df' must be provided"
  )
})

test_that("plot_exposure handles data frame with wrong column names", {
  wrong_cols_data <- data.frame(
    wrong_date = as.Date(c("2023-01-01", "2023-01-02")),
    wrong_atl = c(10, 12),
    wrong_ctl = c(8, 9),
    wrong_acwr = c(1.25, 1.33),
    stringsAsFactors = FALSE
  )
  
  expect_warning(
    p_wrong_cols <- plot_exposure(exposure_df = wrong_cols_data),
    regexp = "No valid exposure data available to plot"
  )
  expect_s3_class(p_wrong_cols, "ggplot")
})

test_that("plot_exposure handles data with all same values", {
  same_values_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    atl = c(10, 10, 10),
    ctl = c(8, 8, 8),
    acwr = c(1.25, 1.25, 1.25),
    stringsAsFactors = FALSE
  )
  
  p_same <- plot_exposure(exposure_df = same_values_data, risk_zones = TRUE)
  expect_s3_class(p_same, "ggplot")
})

test_that("plot_exposure handles extreme ACWR values", {
  extreme_acwr_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    atl = c(10, 12, 11),
    ctl = c(8, 9, 10),
    acwr = c(0.1, 5.0, 10.0),  # Very low and very high ACWR values
    stringsAsFactors = FALSE
  )
  
  p_extreme <- plot_exposure(exposure_df = extreme_acwr_data, risk_zones = TRUE)
  expect_s3_class(p_extreme, "ggplot")
})

test_that("plot_exposure handles data with missing latest point", {
  # Test edge case where max(date) might not be unique
  duplicate_date_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-02")),
    atl = c(10, 11, 12),
    ctl = c(8, 9, 10),
    acwr = c(1.25, 1.22, 1.2),
    stringsAsFactors = FALSE
  )
  
  p_duplicate <- plot_exposure(exposure_df = duplicate_date_data, risk_zones = TRUE)
  expect_s3_class(p_duplicate, "ggplot")
})
