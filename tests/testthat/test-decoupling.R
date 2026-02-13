# tests/testthat/test-decoupling.R
# Tests for calculate_decoupling and plot_decoupling

test_that("sample_decoupling has expected structure", {
  data(sample_decoupling)
  expect_s3_class(sample_decoupling, "data.frame")
  expect_contains(names(sample_decoupling), c("date", "decoupling"))
})

test_that("plot_decoupling handles add_trend_line argument", {
  data(sample_decoupling)
  p_no_trend <- plot_decoupling(data = sample_decoupling, add_trend_line = FALSE)
  p_with_trend <- plot_decoupling(data = sample_decoupling, add_trend_line = TRUE)
  has_smooth <- any(sapply(p_with_trend$layers, function(l) inherits(l$geom, "GeomSmooth")))
  expect_true(has_smooth)
  expect_gte(length(p_with_trend$layers), length(p_no_trend$layers))
})

test_that("plot_decoupling handles empty data frame input", {
  empty_df <- data.frame(
    date = as.Date(character()),
    decoupling = numeric()
  )
  expect_error(
    plot_decoupling(data = empty_df),
    regexp = "Input data frame is empty"
  )
})

test_that("plot_decoupling handles invalid input", {
  expect_error(
    plot_decoupling(data = "not_a_df"),
    regexp = "must be a data frame"
  )
})

test_that("plot_decoupling handles missing columns", {
  bad_df <- data.frame(date = Sys.Date(), other_col = 1)
  expect_error(
    plot_decoupling(data = bad_df),
    regexp = "date.*decoupling"
  )
})

test_that("plot_decoupling accepts custom title and subtitle", {
  data(sample_decoupling)
  p <- plot_decoupling(
    data = sample_decoupling,
    title = "Custom Title",
    subtitle = "Custom Subtitle"
  )
  expect_equal(p$labels$title, "Custom Title")
  expect_equal(p$labels$subtitle, "Custom Subtitle")
})

# --- calculate_decoupling (stream-level) ---

test_that("calculate_decoupling works with simulated stream data", {
  set.seed(501)
  steady_stream <- create_activity_stream(duration_seconds = 3600, steady_state = TRUE)

  decoupling_steady <- calculate_decoupling(
    stream_df = steady_stream,
    decouple_metric = "speed_hr"
  )

  expect_type(decoupling_steady, "double")
  expect_true(is.finite(decoupling_steady))
  expect_true(decoupling_steady > -20 && decoupling_steady < 20)

  decoupling_power <- calculate_decoupling(
    stream_df = steady_stream,
    decouple_metric = "power_hr"
  )

  expect_type(decoupling_power, "double")
  expect_true(is.finite(decoupling_power))
})

test_that("calculate_decoupling handles non-steady state", {
  set.seed(502)
  variable_stream <- create_activity_stream(duration_seconds = 3600, steady_state = FALSE)

  decoupling <- calculate_decoupling(
    stream_df = variable_stream,
    decouple_metric = "speed_hr"
  )

  expect_type(decoupling, "double")
  expect_true(is.finite(decoupling) || is.na(decoupling))
})

test_that("calculate_decoupling works with inst/extdata activity files", {
  activities <- load_extdata_activities()
  decoupling_result <- suppressWarnings(calculate_decoupling(
    activities_data = activities,
    export_dir = extdata_dir,
    activity_type = "Run",
    decouple_metric = "speed_hr",
    min_duration_mins = 20,
    start_date = "2025-01-01", end_date = "2025-01-31"
  ))
  expect_s3_class(decoupling_result, "data.frame")
  expect_contains(names(decoupling_result), c("date", "decoupling", "status"))
})
