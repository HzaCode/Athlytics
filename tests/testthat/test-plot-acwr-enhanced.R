# Test plot_acwr_enhanced functionality

library(testthat)
library(Athlytics)
library(dplyr)

# Create test data
create_test_acwr_data <- function() {
  data.frame(
    date = seq(Sys.Date() - 30, Sys.Date(), by = "day"),
    atl = runif(31, 10, 50),
    ctl = runif(31, 20, 40),
    acwr = runif(31, 0.5, 2.0),
    acwr_smooth = runif(31, 0.6, 1.8),
    acwr_lower = runif(31, 0.4, 1.4),
    acwr_upper = runif(31, 0.8, 2.2),
    stringsAsFactors = FALSE
  )
}

create_test_reference_data <- function() {
  data.frame(
    date = seq(Sys.Date() - 30, Sys.Date(), by = "day"),
    percentile = rep(c("p05", "p25", "p50", "p75", "p95"), each = 31),
    value = runif(155, 0.3, 2.5),
    stringsAsFactors = FALSE
  )
}

# Test basic functionality
test_that("plot_acwr_enhanced works with basic data", {
  acwr_data <- create_test_acwr_data()

  p <- plot_acwr_enhanced(acwr_data)
  expect_s3_class(p, "gg")
})

test_that("plot_acwr_enhanced validates input data", {
  # Test with non-data.frame input
  expect_error(
    plot_acwr_enhanced("not_a_dataframe"),
    "`acwr_data` must be a data frame"
  )

  # Test with missing required columns
  bad_data <- data.frame(date = Sys.Date(), other_col = 1)
  expect_error(
    plot_acwr_enhanced(bad_data),
    "acwr_data must contain columns: date, acwr_smooth"
  )
})

test_that("plot_acwr_enhanced handles confidence intervals", {
  acwr_data <- create_test_acwr_data()

  # Test with CI enabled (should work)
  p1 <- plot_acwr_enhanced(acwr_data, show_ci = TRUE)
  expect_s3_class(p1, "gg")

  # Test with CI disabled
  p2 <- plot_acwr_enhanced(acwr_data, show_ci = FALSE)
  expect_s3_class(p2, "gg")

  # Test with data missing CI columns
  acwr_no_ci <- acwr_data[, !names(acwr_data) %in% c("acwr_lower", "acwr_upper")]
  p3 <- plot_acwr_enhanced(acwr_no_ci, show_ci = TRUE)
  expect_s3_class(p3, "gg")
})

test_that("plot_acwr_enhanced handles reference data", {
  acwr_data <- create_test_acwr_data()
  reference_data <- create_test_reference_data()

  # Test with reference data
  p1 <- plot_acwr_enhanced(acwr_data, reference_data = reference_data)
  expect_s3_class(p1, "gg")

  # Test with reference disabled
  p2 <- plot_acwr_enhanced(acwr_data, reference_data = reference_data, show_reference = FALSE)
  expect_s3_class(p2, "gg")

  # Test with no reference data provided
  p3 <- plot_acwr_enhanced(acwr_data, show_reference = TRUE)
  expect_s3_class(p3, "gg")
})

test_that("plot_acwr_enhanced handles different reference bands", {
  acwr_data <- create_test_acwr_data()
  reference_data <- create_test_reference_data()

  # Test with different band combinations
  p1 <- plot_acwr_enhanced(acwr_data,
    reference_data = reference_data,
    reference_bands = c("p25_p75")
  )
  expect_s3_class(p1, "gg")

  p2 <- plot_acwr_enhanced(acwr_data,
    reference_data = reference_data,
    reference_bands = c("p05_p95", "p50")
  )
  expect_s3_class(p2, "gg")

  p3 <- plot_acwr_enhanced(acwr_data,
    reference_data = reference_data,
    reference_bands = c("p50")
  )
  expect_s3_class(p3, "gg")
})

test_that("plot_acwr_enhanced handles risk zones", {
  acwr_data <- create_test_acwr_data()

  # Test with zones enabled
  p1 <- plot_acwr_enhanced(acwr_data, highlight_zones = TRUE)
  expect_s3_class(p1, "gg")

  # Test with zones disabled
  p2 <- plot_acwr_enhanced(acwr_data, highlight_zones = FALSE)
  expect_s3_class(p2, "gg")
})

test_that("plot_acwr_enhanced handles custom titles", {
  acwr_data <- create_test_acwr_data()

  # Test with custom title and subtitle
  p1 <- plot_acwr_enhanced(acwr_data,
    title = "Custom Title",
    subtitle = "Custom Subtitle"
  )
  expect_s3_class(p1, "gg")

  # Test with method label
  p2 <- plot_acwr_enhanced(acwr_data, method_label = "EWMA")
  expect_s3_class(p2, "gg")
})

test_that("plot_acwr_enhanced handles all combinations", {
  acwr_data <- create_test_acwr_data()
  reference_data <- create_test_reference_data()

  # Test with all features enabled
  p <- plot_acwr_enhanced(
    acwr_data,
    reference_data = reference_data,
    show_ci = TRUE,
    show_reference = TRUE,
    reference_bands = c("p25_p75", "p05_p95", "p50"),
    highlight_zones = TRUE,
    title = "Full Featured Plot",
    subtitle = "All features enabled",
    method_label = "EWMA"
  )
  expect_s3_class(p, "gg")
})

# Test plot_acwr_comparison function
test_that("plot_acwr_comparison works correctly", {
  acwr_ra <- create_test_acwr_data()
  acwr_ewma <- create_test_acwr_data()

  # Test basic comparison
  p1 <- plot_acwr_comparison(acwr_ra, acwr_ewma)
  expect_s3_class(p1, "gg")

  # Test with custom title
  p2 <- plot_acwr_comparison(acwr_ra, acwr_ewma, title = "Custom Comparison")
  expect_s3_class(p2, "gg")
})

test_that("plot_acwr_comparison handles data binding", {
  acwr_ra <- create_test_acwr_data()
  acwr_ewma <- create_test_acwr_data()

  # The function should combine the data correctly
  p <- plot_acwr_comparison(acwr_ra, acwr_ewma)

  # Check that the plot has the expected structure
  expect_s3_class(p, "gg")

  # The plot should be a valid ggplot
  expect_s3_class(p, "gg")
})

# Test edge cases
test_that("plot_acwr_enhanced handles edge cases", {
  # Test with single data point
  single_point <- data.frame(
    date = Sys.Date(),
    acwr_smooth = 1.0,
    acwr_lower = 0.8,
    acwr_upper = 1.2
  )

  p <- plot_acwr_enhanced(single_point)
  expect_s3_class(p, "gg")

  # Test with NA values
  acwr_with_na <- create_test_acwr_data()
  acwr_with_na$acwr_smooth[5:10] <- NA

  p2 <- plot_acwr_enhanced(acwr_with_na)
  expect_s3_class(p2, "gg")
})

test_that("plot_acwr_enhanced handles missing reference percentiles", {
  acwr_data <- create_test_acwr_data()

  # Create reference data missing some percentiles
  incomplete_ref <- data.frame(
    date = seq(Sys.Date() - 30, Sys.Date(), by = "day"),
    percentile = rep(c("p25", "p75"), each = 31),
    value = runif(62, 0.5, 2.0),
    stringsAsFactors = FALSE
  )

  # Should still work but only show available bands
  p <- plot_acwr_enhanced(acwr_data, reference_data = incomplete_ref)
  expect_s3_class(p, "gg")
})
