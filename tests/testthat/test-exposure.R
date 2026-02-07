# tests/testthat/test-exposure.R

library(testthat)
library(Athlytics)
library(dplyr)

# Load sample data from the package
data(sample_acwr)
data(sample_exposure)

# Load mock data (if helper-mockdata.R contains mocks for direct use)
source(test_path("helper-mockdata.R"), local = TRUE)


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
    # Refactored: Pass as first argument (data)
    expect_s3_class(plot_exposure(sample_acwr), "ggplot")
  } else {
    skip("sample_acwr is empty, skipping main plot test.")
  }
})

test_that("plot_exposure handles risk_zones argument with sample_acwr", {
  if (!exists("sample_acwr") || nrow(sample_acwr) == 0) {
    skip("sample_acwr is empty or not found, skipping risk_zones test.")
  }
  expect_s3_class(plot_exposure(sample_acwr, risk_zones = TRUE), "ggplot")
  expect_s3_class(plot_exposure(sample_acwr, risk_zones = FALSE), "ggplot")

  # Check for geom_rect layers (risk zones are often drawn with geom_rect for background bands)
  # or geom_hline/geom_vline if specific lines are used.
  # The original test checked for GeomAbline, let's adapt if needed or keep if appropriate.
  # plot_exposure uses geom_abline for risk zones.
  p_zones <- plot_exposure(sample_acwr, risk_zones = TRUE)
  p_no_zones <- plot_exposure(sample_acwr, risk_zones = FALSE)

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

  expect_error(
    plot_exposure(empty_df),
    regexp = "Input data frame is empty."
  )
})

# ============================================================
# Numerical Value Validation
# ============================================================

test_that("calculate_exposure produces correct ATL/CTL with constant load", {
  end_date <- Sys.Date()
  start_date <- end_date - 120
  dates <- seq(start_date, end_date, by = "day")
  n <- length(dates)

  activities <- data.frame(
    id = seq_len(n), name = paste("Run", seq_len(n)),
    type = "Run", sport_type = "Run",
    date = dates, start_date_local = as.POSIXct(dates),
    distance = rep(10000, n), moving_time = rep(3600, n),
    elapsed_time = rep(3600, n), average_heartrate = rep(150, n),
    average_speed = rep(3.0, n), stringsAsFactors = FALSE
  )

  result <- calculate_exposure(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "duration_mins",
    end_date = end_date
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("date", "atl", "ctl") %in% names(result)))
  expect_true("athlytics_exposure" %in% class(result))

  # With constant load, ATL and CTL should converge to 60
  late_rows <- result %>% filter(date >= (end_date - 30))
  if (nrow(late_rows) > 0) {
    expect_true(all(late_rows$atl > 0, na.rm = TRUE))
    expect_true(all(late_rows$ctl > 0, na.rm = TRUE))
    # Both should be close to 60
    expect_true(
      mean(abs(late_rows$atl - 60), na.rm = TRUE) < 5,
      info = "ATL should be close to 60 for constant 60min/day"
    )
    expect_true(
      mean(abs(late_rows$ctl - 60), na.rm = TRUE) < 5,
      info = "CTL should be close to 60 for constant 60min/day"
    )
  }
})
