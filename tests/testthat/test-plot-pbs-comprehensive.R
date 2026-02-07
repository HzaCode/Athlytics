# Comprehensive test for plot_pbs.R to boost coverage

test_that("plot_pbs handles pre-calculated data", {
  data("sample_pbs")

  # Test basic plotting with pre-calculated data
  p1 <- plot_pbs(data = sample_pbs)
  expect_s3_class(p1, "ggplot")
  expect_true(length(p1$layers) >= 1)
  expect_equal(p1$labels$x, "Activity Date")
  expect_equal(p1$labels$y, "Best Time (MM:SS)")
  expect_true(grepl("Personal Best", p1$labels$title))

  # Verify plot data contains all distances from input
  expect_equal(sort(unique(p1$data$distance)), sort(unique(sample_pbs$distance)))

  # Test with add_trend_line = FALSE
  p2 <- plot_pbs(data = sample_pbs, add_trend_line = FALSE)
  expect_s3_class(p2, "ggplot")
  has_smooth_2 <- any(sapply(p2$layers, function(l) inherits(l$geom, "GeomSmooth")))
  expect_false(has_smooth_2)

  # Test with add_trend_line = TRUE (should have GeomSmooth layer)
  p3 <- plot_pbs(data = sample_pbs, add_trend_line = TRUE)
  expect_s3_class(p3, "ggplot")
  has_smooth_3 <- any(sapply(p3$layers, function(l) inherits(l$geom, "GeomSmooth")))
  expect_true(has_smooth_3, info = "Trend line should add a GeomSmooth layer")
  expect_true(length(p3$layers) > length(p2$layers))
})

test_that("plot_pbs handles edge cases", {
  # Test with empty PBS data
  empty_pbs <- data.frame(
    activity_date = lubridate::as_date(character(0)),
    distance = numeric(0),
    time_seconds = numeric(0),
    is_pb = logical(0)
  )

  expect_error(
    plot_pbs(data = empty_pbs),
    regexp = "No PB data available to plot"
  )

  # Test with single data point
  single_pbs <- data.frame(
    activity_date = Sys.Date(),
    distance = 1000,
    time_seconds = 300,
    is_pb = TRUE
  )

  p_single <- plot_pbs(data = single_pbs)
  expect_s3_class(p_single, "ggplot")
  expect_true(length(p_single$layers) >= 1)
})

test_that("plot_pbs handles missing columns gracefully", {
  # Test with missing is_pb column
  pbs_no_pb <- data.frame(
    activity_date = c(Sys.Date(), Sys.Date() - 1),
    distance = c(1000, 1000),
    time_seconds = c(300, 310)
  )

  p_no_pb <- plot_pbs(data = pbs_no_pb)
  expect_s3_class(p_no_pb, "ggplot")
  expect_true(length(p_no_pb$layers) >= 1)
})

test_that("plot_pbs handles data with different structures", {
  # Test with minimal required columns
  minimal_pbs <- data.frame(
    activity_date = c(Sys.Date(), Sys.Date() - 1),
    distance = c(1000, 1000),
    time_seconds = c(300, 310),
    is_pb = c(TRUE, FALSE)
  )

  p_minimal <- plot_pbs(data = minimal_pbs)
  expect_s3_class(p_minimal, "ggplot")
  expect_true(nrow(p_minimal$data) > 0)

  # Test with extra columns
  extra_pbs <- data.frame(
    activity_date = c(Sys.Date(), Sys.Date() - 1),
    distance = c(1000, 1000),
    time_seconds = c(300, 310),
    is_pb = c(TRUE, FALSE),
    extra_col = c("A", "B")
  )

  p_extra <- plot_pbs(data = extra_pbs)
  expect_s3_class(p_extra, "ggplot")
  expect_true(nrow(p_extra$data) > 0)
})

test_that("plot_pbs facets for multiple distances", {
  multi_dist_pbs <- data.frame(
    activity_date = as.Date(c("2023-01-01", "2023-01-08", "2023-01-01")),
    time_seconds = c(1200, 1180, 300),
    distance = c(5000, 5000, 1000),
    is_pb = c(TRUE, TRUE, TRUE),
    distance_label = factor(c("5k", "5k", "1k"), levels = c("1k", "5k")),
    stringsAsFactors = FALSE
  )

  p_multi <- plot_pbs(data = multi_dist_pbs)
  is_faceted <- inherits(p_multi$facet, "FacetWrap")
  expect_true(is_faceted)

  # Verify facet uses free_y scales
  facet_params <- p_multi$facet$params
  expect_equal(facet_params$free$y, TRUE,
    info = "Faceted PB plot should use free_y scales"
  )
})

test_that("plot_pbs custom title and caption work", {
  data("sample_pbs")

  p <- plot_pbs(
    data = sample_pbs,
    title = "Custom Title",
    subtitle = "Custom Sub",
    caption = "Test caption"
  )
  expect_equal(p$labels$title, "Custom Title")
  expect_equal(p$labels$subtitle, "Custom Sub")
  expect_equal(p$labels$caption, "Test caption")
})
