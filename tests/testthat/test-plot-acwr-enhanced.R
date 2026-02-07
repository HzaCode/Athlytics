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
  expect_true(length(p$layers) >= 1)
  expect_equal(p$labels$x, "Date")
  expect_equal(p$labels$y, "ACWR (Smoothed)")
  expect_true(grepl("ACWR", p$labels$title))
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
    "must be the output of calculate_acwr_ewma"
  )
})

test_that("plot_acwr_enhanced handles confidence intervals", {
  acwr_data <- create_test_acwr_data()

  # Test with CI enabled (should have ribbon layer)
  p1 <- plot_acwr_enhanced(acwr_data, show_ci = TRUE)
  expect_s3_class(p1, "gg")
  layer_geoms_1 <- sapply(p1$layers, function(l) class(l$geom)[1])
  expect_true("GeomRibbon" %in% layer_geoms_1)

  # Test with CI disabled (fewer ribbon layers expected)
  p2 <- plot_acwr_enhanced(acwr_data, show_ci = FALSE)
  expect_s3_class(p2, "gg")
  layer_geoms_2 <- sapply(p2$layers, function(l) class(l$geom)[1])
  n_ribbon_2 <- sum(layer_geoms_2 == "GeomRibbon")
  n_ribbon_1 <- sum(layer_geoms_1 == "GeomRibbon")
  expect_true(n_ribbon_2 < n_ribbon_1)

  # Test with data missing CI columns (should fall back gracefully)
  acwr_no_ci <- acwr_data[, !names(acwr_data) %in% c("acwr_lower", "acwr_upper")]
  p3 <- plot_acwr_enhanced(acwr_no_ci, show_ci = TRUE)
  expect_s3_class(p3, "gg")
  layer_geoms_3 <- sapply(p3$layers, function(l) class(l$geom)[1])
  # Without CI columns, no CI ribbon should be present
  expect_true(sum(layer_geoms_3 == "GeomRibbon") <= n_ribbon_2)
})

test_that("plot_acwr_enhanced handles reference data", {
  acwr_data <- create_test_acwr_data()
  reference_data <- create_test_reference_data()

  # Test with reference data (should add extra layers)
  p1 <- plot_acwr_enhanced(acwr_data, reference_data = reference_data)
  expect_s3_class(p1, "gg")
  expect_true(length(p1$layers) >= 2)

  # Test with reference disabled
  p2 <- plot_acwr_enhanced(acwr_data, reference_data = reference_data, show_reference = FALSE)
  expect_s3_class(p2, "gg")

  # Test with no reference data provided
  p3 <- plot_acwr_enhanced(acwr_data, show_reference = TRUE)
  expect_s3_class(p3, "gg")
  n_layers_no_ref <- length(p3$layers)
  expect_true(n_layers_no_ref <= length(p1$layers))
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
  expect_true(length(p1$layers) >= 1)

  p2 <- plot_acwr_enhanced(acwr_data,
    reference_data = reference_data,
    reference_bands = c("p05_p95", "p50")
  )
  expect_s3_class(p2, "gg")
  expect_true(length(p2$layers) >= length(p1$layers))

  p3 <- plot_acwr_enhanced(acwr_data,
    reference_data = reference_data,
    reference_bands = c("p50")
  )
  expect_s3_class(p3, "gg")
  expect_true(length(p3$layers) >= 1)
})

test_that("plot_acwr_enhanced handles risk zones", {
  acwr_data <- create_test_acwr_data()

  # Test with zones enabled
  p1 <- plot_acwr_enhanced(acwr_data, highlight_zones = TRUE)
  expect_s3_class(p1, "gg")
  n_layers_zones <- length(p1$layers)

  # Verify risk zone adds rect/annotate layers
  layer_geoms_1 <- sapply(p1$layers, function(l) class(l$geom)[1])
  expect_true("GeomRect" %in% layer_geoms_1 || "GeomAnnotation" %in% layer_geoms_1,
    info = "Risk zones should add rect or annotation layers"
  )

  # Test with zones disabled (fewer layers expected)
  p2 <- plot_acwr_enhanced(acwr_data, highlight_zones = FALSE)
  expect_s3_class(p2, "gg")
  expect_true(length(p2$layers) < n_layers_zones,
    info = "Disabling risk zones should result in fewer layers"
  )
})

test_that("plot_acwr_enhanced handles custom titles", {
  acwr_data <- create_test_acwr_data()

  # Test with custom title and subtitle
  p1 <- plot_acwr_enhanced(acwr_data,
    title = "Custom Title",
    subtitle = "Custom Subtitle"
  )
  expect_s3_class(p1, "gg")
  expect_equal(p1$labels$title, "Custom Title")
  expect_equal(p1$labels$subtitle, "Custom Subtitle")

  # Test with method label
  p2 <- plot_acwr_enhanced(acwr_data, method_label = "EWMA")
  expect_s3_class(p2, "gg")
  expect_true(grepl("EWMA", p2$labels$title) || grepl("EWMA", p2$labels$subtitle %||% ""))
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
  expect_true(length(p$layers) >= 3)
  expect_equal(p$labels$title, "Full Featured Plot")
})

# Test plot_acwr_comparison function
test_that("plot_acwr_comparison works correctly", {
  acwr_ra <- create_test_acwr_data()
  acwr_ewma <- create_test_acwr_data()

  # Test basic comparison
  p1 <- plot_acwr_comparison(acwr_ra, acwr_ewma)
  expect_s3_class(p1, "gg")
  expect_true(length(p1$layers) >= 2)

  # Test with custom title
  p2 <- plot_acwr_comparison(acwr_ra, acwr_ewma, title = "Custom Comparison")
  expect_s3_class(p2, "gg")
  expect_equal(p2$labels$title, "Custom Comparison")
})

test_that("plot_acwr_comparison handles data binding", {
  acwr_ra <- create_test_acwr_data()
  acwr_ewma <- create_test_acwr_data()

  # The function should combine the data correctly
  p <- plot_acwr_comparison(acwr_ra, acwr_ewma)
  expect_s3_class(p, "gg")
  # Combined data should have rows from both sources
  expect_equal(nrow(p$data), nrow(acwr_ra) + nrow(acwr_ewma))
  # Should have a method column distinguishing the two datasets
  expect_true("method" %in% names(p$data),
    info = "Combined data should have a 'method' column"
  )
  # Verify both methods are represented
  expect_equal(length(unique(p$data$method)), 2,
    info = "Should have exactly 2 methods in combined data"
  )
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
  expect_true(length(p$layers) >= 1)

  # Test with NA values
  acwr_with_na <- create_test_acwr_data()
  acwr_with_na$acwr_smooth[5:10] <- NA

  p2 <- plot_acwr_enhanced(acwr_with_na)
  expect_s3_class(p2, "gg")
  expect_true(length(p2$layers) >= 1)
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
  expect_true(length(p$layers) >= 1)
})
