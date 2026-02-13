# Test plot_acwr_enhanced behavior (visual output is covered by vdiffr)
# Uses sample_acwr instead of synthetic mock data

layer_geoms <- function(plot_obj) {
  vapply(plot_obj$layers, function(l) class(l$geom)[1], character(1))
}

# Load sample data and add CI columns for testing
data("sample_acwr")
acwr_with_ci <- sample_acwr
if (!"acwr_lower" %in% names(acwr_with_ci)) {
  acwr_with_ci$acwr_lower <- acwr_with_ci$acwr_smooth * 0.8
}
if (!"acwr_upper" %in% names(acwr_with_ci)) {
  acwr_with_ci$acwr_upper <- acwr_with_ci$acwr_smooth * 1.2
}

test_that("plot_acwr_enhanced validates input data", {
  expect_error(
    plot_acwr_enhanced("not_a_dataframe"),
    "`acwr_data` must be a data frame"
  )

  bad_data <- data.frame(date = Sys.Date(), other_col = 1)
  expect_error(
    plot_acwr_enhanced(bad_data),
    "must be the output of calculate_acwr_ewma"
  )
})

test_that("plot_acwr_enhanced returns drawable object with required geoms", {
  p <- plot_acwr_enhanced(acwr_with_ci)
  geoms <- layer_geoms(p)
  expect_true("GeomLine" %in% geoms || "GeomPoint" %in% geoms)
})

test_that("plot_acwr_enhanced handles confidence interval option", {
  p_ci <- plot_acwr_enhanced(acwr_with_ci, show_ci = TRUE)
  p_no_ci <- plot_acwr_enhanced(acwr_with_ci, show_ci = FALSE)
  expect_true("GeomRibbon" %in% layer_geoms(p_ci))
  expect_false("GeomRibbon" %in% layer_geoms(p_no_ci))

  # When CI columns are absent, ribbon should not appear even if requested
  acwr_no_ci <- acwr_with_ci[, !names(acwr_with_ci) %in% c("acwr_lower", "acwr_upper")]
  p_missing_ci <- plot_acwr_enhanced(acwr_no_ci, show_ci = TRUE)
  expect_false("GeomRibbon" %in% layer_geoms(p_missing_ci))
})

test_that("plot_acwr_enhanced handles reference overlays", {
  # Create minimal reference data aligned with sample_acwr dates
  n_dates <- min(30, nrow(sample_acwr))
  ref_dates <- sample_acwr$date[1:n_dates]
  reference_data <- data.frame(
    date = rep(ref_dates, times = 5),
    percentile = rep(c("p05", "p25", "p50", "p75", "p95"), each = n_dates),
    value = runif(5 * n_dates, 0.3, 2.5),
    stringsAsFactors = FALSE
  )

  p_with_ref <- plot_acwr_enhanced(
    acwr_with_ci,
    reference_data = reference_data,
    show_reference = TRUE,
    reference_bands = c("p25_p75", "p50")
  )
  expect_true(any(layer_geoms(p_with_ref) %in% c("GeomLine", "GeomRibbon", "GeomRect")))

  # Incomplete reference (missing some percentiles)
  incomplete_ref <- data.frame(
    date = rep(ref_dates, times = 2),
    percentile = rep(c("p25", "p75"), each = n_dates),
    value = runif(2 * n_dates, 0.5, 2.0),
    stringsAsFactors = FALSE
  )
  p_incomplete <- plot_acwr_enhanced(acwr_with_ci, reference_data = incomplete_ref)
  expect_true(any(layer_geoms(p_incomplete) %in% c("GeomLine", "GeomRibbon", "GeomRect")))
})

test_that("plot_acwr_enhanced handles risk zones and custom labels", {
  p_zone <- plot_acwr_enhanced(acwr_with_ci, highlight_zones = TRUE)
  expect_true(any(layer_geoms(p_zone) %in% c("GeomRect", "GeomAnnotation")))

  p_labels <- plot_acwr_enhanced(
    acwr_with_ci,
    title = "Custom Title",
    subtitle = "Custom Subtitle",
    method_label = "EWMA"
  )
  expect_equal(p_labels$labels$title, "Custom Title")
  expect_equal(p_labels$labels$subtitle, "Custom Subtitle")
})

test_that("plot_acwr_comparison combines both methods correctly", {
  acwr_ra <- sample_acwr
  acwr_ewma <- sample_acwr
  set.seed(42)
  acwr_ewma$acwr_smooth <- acwr_ewma$acwr_smooth * runif(nrow(acwr_ewma), 0.95, 1.05)

  p <- plot_acwr_comparison(acwr_ra, acwr_ewma)
  expect_equal(nrow(p$data), nrow(acwr_ra) + nrow(acwr_ewma))
  expect_contains(names(p$data), "method")
  expect_length(unique(p$data$method), 2)

  p_custom <- plot_acwr_comparison(acwr_ra, acwr_ewma, title = "Custom Comparison")
  expect_equal(p_custom$labels$title, "Custom Comparison")
})

test_that("plot_acwr_enhanced handles sparse and NA data", {
  # Single data point
  single_point <- sample_acwr[1, , drop = FALSE]
  p_single <- plot_acwr_enhanced(single_point)
  expect_true(any(layer_geoms(p_single) %in% c("GeomLine", "GeomPoint")))

  # Data with NA values
  acwr_with_na <- sample_acwr
  na_range <- 5:min(10, nrow(acwr_with_na))
  acwr_with_na$acwr_smooth[na_range] <- NA
  p_na <- plot_acwr_enhanced(acwr_with_na)
  expect_true(any(layer_geoms(p_na) %in% c("GeomLine", "GeomPoint")))
})
