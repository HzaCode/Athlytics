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

# ============================================================
# Regression tests — bug-fix guards grouped by version
# ============================================================

# --- quality_control = "flag" must not behave like "filter" (v1.0.4) ----

test_that("quality_control = 'flag' keeps rows instead of dropping them (regression)", {
  # Build a stream where a chunk of rows would be out-of-range under the
  # quality gate. "flag" must keep those rows (identical to "off") while
  # "filter" must remove them (and therefore produce a different number).
  n <- 3600
  set.seed(3)
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = c(
      rep(150, 3400) + rnorm(3400, 0, 0.5),
      rep(230, 200) # 5% of HR values are out-of-range
    ),
    velocity_smooth = rep(3.0, n) + rnorm(n, 0, 0.01),
    distance = cumsum(rep(3.0, n)),
    watts = rep(250, n) + rnorm(n, 0, 1)
  )
  stream <- stream[order(stream$time), ]

  val_filter <- suppressMessages(calculate_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr",
    quality_control = "filter",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.5
  ))

  val_flag <- suppressMessages(calculate_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr",
    quality_control = "flag",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.5
  ))

  val_off <- suppressMessages(calculate_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr",
    quality_control = "off",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.5
  ))

  expect_equal(val_flag, val_off,
    info = "flag and off should produce identical decoupling (no rows dropped)"
  )
  expect_false(isTRUE(all.equal(val_flag, val_filter)),
    info = "flag must NOT behave like filter"
  )
})

# --- calculate_decoupling(stream_df=...) forwards user params (v1.0.4) --

test_that("calculate_decoupling(stream_df=...) forwards min_hr_coverage (regression)", {
  n <- 3600
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = c(rep(150, n / 2), rep(NA_real_, n / 2)),
    velocity_smooth = rep(3.0, n),
    distance = cumsum(rep(3.0, n))
  )

  val_strict <- suppressMessages(calculate_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr",
    min_hr_coverage = 0.9,
    min_steady_minutes = 5,
    steady_cv_threshold = 0.2
  ))
  expect_true(is.na(val_strict),
    info = "strict min_hr_coverage=0.9 should reject 50% HR coverage"
  )
})

test_that("calculate_decoupling(stream_df=...) forwards min_steady_minutes (regression)", {
  n <- 1800
  set.seed(4)
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = rep(150, n) + rnorm(n, 0, 0.5),
    velocity_smooth = rep(3.0, n) + rnorm(n, 0, 0.01),
    distance = cumsum(rep(3.0, n))
  )

  val_strict <- suppressMessages(calculate_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr",
    min_steady_minutes = 40,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.5
  ))
  expect_true(is.na(val_strict),
    info = "min_steady_minutes > stream duration should reject"
  )

  val_lax <- suppressMessages(calculate_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.5
  ))
  expect_false(is.na(val_lax))
})

# --- zip support (v1.0.4) -----------------------------------------------

test_that("calculate_decoupling accepts zip export_dir (regression)", {
  zip_path <- make_extdata_zip()
  on.exit(unlink(zip_path), add = TRUE)

  activities <- suppressWarnings(load_local_activities(zip_path))

  result <- suppressWarnings(tryCatch(
    calculate_decoupling(
      activities_data = activities,
      export_dir = zip_path,
      activity_type = "Run",
      decouple_metric = "speed_hr",
      min_duration_mins = 1,
      min_steady_minutes = 1,
      steady_cv_threshold = 0.5,
      min_hr_coverage = 0.3
    ),
    error = function(e) e
  ))

  # Post-fix: either a data frame (possibly empty) or a different error,
  # but NOT the dir.exists-style "directory or .zip" rejection.
  if (inherits(result, "error")) {
    expect_false(grepl("directory or a .zip file", conditionMessage(result)))
  } else {
    expect_s3_class(result, "data.frame")
  }
})

# --- Continuous-block steady-state logic (v1.0.4) -----------------------

test_that("calculate_decoupling does not treat separated steady islands as one block (regression)", {
  # Two 15-min steady islands separated by 30 min of chaos. Pre-fix the
  # max(time) - min(time) span included BOTH islands, so the first/second-
  # half split compared unrelated segments. Post-fix, the longest single
  # contiguous steady block is only 15 minutes, below min_steady_minutes = 25.
  set.seed(7)
  island1_n <- 900
  chaos_n <- 1800
  island2_n <- 900
  n <- island1_n + chaos_n + island2_n

  velocity <- c(
    rep(3.0, island1_n) + rnorm(island1_n, 0, 0.005),
    runif(chaos_n, 1.0, 6.0),
    rep(3.0, island2_n) + rnorm(island2_n, 0, 0.005)
  )
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = rep(150, n) + rnorm(n, 0, 0.5),
    velocity_smooth = velocity,
    distance = cumsum(velocity)
  )

  val <- suppressMessages(calculate_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr",
    min_steady_minutes = 25,
    steady_cv_threshold = 0.05,
    min_hr_coverage = 0.5
  ))
  expect_true(is.na(val),
    info = "Two 15-min islands must not be merged into one 'block'"
  )
})

# --- Legacy capitalized metric names (v1.0.4) ---------------------------

test_that("calculate_decoupling accepts legacy capitalized decouple_metric (regression)", {
  set.seed(8)
  n <- 3600
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = rep(150, n) + rnorm(n, 0, 0.5),
    velocity_smooth = rep(3.0, n) + rnorm(n, 0, 0.01),
    distance = cumsum(rep(3.0, n))
  )

  expect_no_error(suppressMessages(calculate_decoupling(
    stream_df = stream,
    decouple_metric = "Speed_HR",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.5
  )))
})

# --- quality_score propagation via internal list return (v1.0.4) --------

test_that("calculate_decoupling stream path propagates quality_score via list return (regression)", {
  set.seed(202)
  n <- 3600
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = rep(150, n) + rnorm(n, 0, 0.5),
    velocity_smooth = rep(3.0, n) + rnorm(n, 0, 0.01),
    distance = cumsum(rep(3.0, n))
  )

  # Call the internal function directly so we can assert on the structured
  # list rather than the numeric-only user-facing wrapper.
  result <- suppressMessages(Athlytics:::calculate_single_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr",
    quality_control = "filter",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.5
  ))

  expect_true(is.list(result))
  expect_true(all(c("value", "status", "hr_coverage", "quality_score") %in% names(result)))
  expect_true(is.finite(result$hr_coverage))
})

# --- return_diagnostics knob (v1.0.5) -----------------------------------

test_that("calculate_decoupling returns numeric by default and a data frame with diagnostics on request", {
  set.seed(5)
  n <- 3600
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = rep(150, n) + rnorm(n, 0, 1),
    velocity_smooth = rep(3, n) + rnorm(n, 0, 0.02),
    distance = cumsum(rep(3, n)),
    stringsAsFactors = FALSE
  )

  numeric_out <- suppressMessages(calculate_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr",
    min_steady_minutes = 20,
    min_hr_coverage = 0.5
  ))
  expect_true(is.numeric(numeric_out))
  expect_length(numeric_out, 1)

  diag_out <- suppressMessages(calculate_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr",
    min_steady_minutes = 20,
    min_hr_coverage = 0.5,
    return_diagnostics = TRUE
  ))
  expect_s3_class(diag_out, "data.frame")
  expect_true(all(
    c(
      "decoupling", "status", "quality_score", "hr_coverage",
      "steady_duration_minutes", "sampling_interval_seconds"
    ) %in% colnames(diag_out)
  ))
  expect_equal(nrow(diag_out), 1L)
  expect_equal(diag_out$status, "ok")
})
