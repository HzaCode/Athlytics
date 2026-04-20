# tests/testthat/test-ef.R
# Efficiency Factor Calculation Tests
# Uses inst/extdata/activities.csv via load_extdata_activities() from helper-test-data.R

test_that("calculate_ef works with speed_hr metric", {
  activities <- load_extdata_activities()
  win <- extdata_window(activities)

  ef_result <- suppressMessages(calculate_ef(
    activities_data = activities,
    ef_metric = "speed_hr",
    start_date = win$start_date,
    end_date = win$end_date
  ))

  expect_s3_class(ef_result, "data.frame")
  expect_contains(colnames(ef_result), "ef_value")
  expect_gt(nrow(ef_result), 0)
})

test_that("calculate_ef works with power_hr metric", {
  activities <- load_extdata_activities()
  win <- extdata_window(activities)

  ef_result <- suppressMessages(calculate_ef(
    activities_data = activities,
    ef_metric = "power_hr",
    start_date = win$start_date,
    end_date = win$end_date
  ))

  expect_s3_class(ef_result, "data.frame")
  expect_contains(colnames(ef_result), "ef_value")
})

test_that("calculate_ef validates input", {
  expect_error(calculate_ef())
  expect_error(calculate_ef(NULL, activity_type = "Run", ef_metric = "speed_hr"))
  expect_error(
    calculate_ef(activities_data = "not_a_dataframe"),
    "data frame"
  )
  expect_error(calculate_ef(data.frame(), activity_type = "Run", ef_metric = "speed_hr"))

  df <- data.frame(
    id = 1, date = Sys.Date(), type = "Run", moving_time = 1800,
    distance = 5000, average_heartrate = 150, average_speed = 10
  )
  expect_error(calculate_ef(df, activity_type = "Run", ef_metric = "speed_hr", min_duration_mins = -5))
  expect_error(calculate_ef(df, activity_type = "Run", ef_metric = "speed_hr", steady_cv_threshold = 1.5))
  expect_error(calculate_ef(df, activity_type = "Run", ef_metric = "speed_hr", steady_cv_threshold = 0))
  expect_error(calculate_ef(df, activity_type = "Run", ef_metric = "speed_hr", min_hr_coverage = 1.5))
  expect_error(calculate_ef(df, activity_type = "Run", ef_metric = "speed_hr", min_hr_coverage = 0))
  expect_error(calculate_ef(df,
    activity_type = "Run", ef_metric = "speed_hr",
    start_date = Sys.Date(), end_date = Sys.Date() - 30
  ))
})

test_that("calculate_ef works with sample data", {
  data(sample_ef)

  expect_s3_class(sample_ef, "data.frame")
  expect_true(any(c("ef_value", "efficiency_factor") %in% names(sample_ef)))
})

test_that("plot_ef trend line behavior is correct on pre-calculated data", {
  data(sample_ef)

  has_smooth_layer <- function(plot_obj) {
    any(vapply(plot_obj$layers, function(l) inherits(l$geom, "GeomSmooth"), logical(1)))
  }

  p_trend <- plot_ef(sample_ef, add_trend_line = TRUE)
  p_no_trend <- plot_ef(sample_ef, add_trend_line = FALSE)

  expect_true(has_smooth_layer(p_trend))
  expect_false(has_smooth_layer(p_no_trend))
  expect_equal(nrow(p_trend$data), nrow(sample_ef))
})

test_that("plot_ef accepts calculate_ef output and preserves rows", {
  activities <- load_extdata_activities()
  win <- extdata_window(activities)
  ef_data <- suppressMessages(calculate_ef(activities,
    activity_type = "Run", ef_metric = "speed_hr",
    start_date = win$start_date, end_date = win$end_date))

  p_basic <- plot_ef(data = ef_data, add_trend_line = FALSE)
  expect_equal(nrow(p_basic$data), nrow(ef_data))
  expect_contains(names(p_basic$data), c("date", "ef_value"))

  p_lm <- plot_ef(data = ef_data, smoothing_method = "lm")
  expect_true(any(vapply(p_lm$layers, function(l) inherits(l$geom, "GeomSmooth"), logical(1))))
})

# ============================================================
# Numerical Value Validation
# ============================================================

test_that("calculate_ef produces correct speed_hr values", {
  # Known inputs: distance = 10km, time = 3600s (1hr), HR = 150
  # EF = speed / HR = (10000/3600) / 150 = 2.778 / 150 = 0.01852
  end_date <- Sys.Date()
  start_date <- end_date - 9
  dates <- seq(start_date, end_date, by = "day")
  n <- length(dates)

  activities <- data.frame(
    id = seq_len(n),
    name = paste("Run", seq_len(n)),
    type = "Run",
    date = dates,
    start_date_local = as.POSIXct(dates),
    distance = rep(10000, n),
    moving_time = rep(3600, n),
    elapsed_time = rep(3600, n),
    average_heartrate = rep(150, n),
    average_speed = rep(2.778, n),
    filename = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )

  result <- calculate_ef(
    activities_data = activities,
    activity_type = "Run",
    ef_metric = "speed_hr",
    min_duration_mins = 10,
    min_steady_minutes = 10,
    quality_control = "off",
    start_date = start_date,
    end_date = end_date
  )

  expect_gt(nrow(result), 0)

  # Check EF value is close to expected
  valid_ef <- result %>% dplyr::filter(!is.na(ef_value))
  if (nrow(valid_ef) > 0) {
    expected_ef <- (10000 / 3600) / 150
    expect_true(
      all(abs(valid_ef$ef_value - expected_ef) < 0.005),
      info = sprintf(
        "EF should be ~%.4f (speed/HR), got: %s",
        expected_ef, paste(round(valid_ef$ef_value, 4), collapse = ", ")
      )
    )
  }
})

test_that("calculate_ef produces correct power_hr values", {
  # Known inputs: power = 200W, HR = 150
  # EF = 200/150 = 1.333
  end_date <- Sys.Date()
  start_date <- end_date - 9
  dates <- seq(start_date, end_date, by = "day")
  n <- length(dates)

  activities <- data.frame(
    id = seq_len(n),
    name = paste("Ride", seq_len(n)),
    type = "Ride",
    date = dates,
    start_date_local = as.POSIXct(dates),
    distance = rep(30000, n),
    moving_time = rep(3600, n),
    elapsed_time = rep(3600, n),
    average_heartrate = rep(150, n),
    average_watts = rep(200, n),
    weighted_average_watts = rep(0, n),
    average_speed = rep(8.3, n),
    filename = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )

  result <- calculate_ef(
    activities_data = activities,
    activity_type = "Ride",
    ef_metric = "power_hr",
    min_duration_mins = 10,
    min_steady_minutes = 10,
    quality_control = "off",
    start_date = start_date,
    end_date = end_date
  )

  expect_gt(nrow(result), 0)

  valid_ef <- result %>% dplyr::filter(!is.na(ef_value))
  if (nrow(valid_ef) > 0) {
    expected_ef <- 200 / 150
    expect_true(
      all(abs(valid_ef$ef_value - expected_ef) < 0.01),
      info = sprintf("EF should be ~%.3f (power/HR)", expected_ef)
    )
  }
})

test_that("calculate_ef returns athlytics_ef class", {
  end_date <- Sys.Date()
  start_date <- end_date - 9
  dates <- seq(start_date, end_date, by = "day")
  n <- length(dates)
  activities <- data.frame(
    id = seq_len(n), name = paste("Run", seq_len(n)),
    type = "Run", date = dates, start_date_local = as.POSIXct(dates),
    distance = rep(10000, n), moving_time = rep(3600, n),
    elapsed_time = rep(3600, n), average_heartrate = rep(150, n),
    average_speed = rep(2.778, n), filename = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )
  result <- calculate_ef(
    activities_data = activities, activity_type = "Run",
    ef_metric = "speed_hr", quality_control = "off",
    min_duration_mins = 10, min_steady_minutes = 10,
    start_date = start_date, end_date = end_date
  )
  expect_s3_class(result, "athlytics_ef")
})

# ============================================================
# plot_ef Coverage - Validation, Labels, Grouping
# ============================================================

test_that("plot_ef validates input types", {
  expect_error(plot_ef("not_a_df"), "must be a data frame")
  expect_error(plot_ef(data.frame(x = 1)), "must be the output of calculate_ef")
  expect_error(
    plot_ef(data.frame(date = as.Date(character()), ef_value = numeric(), activity_type = character())),
    "empty"
  )
})

test_that("plot_ef warns on deprecated analysis arguments", {
  data(sample_ef)
  expect_warning(
    plot_ef(sample_ef, activity_type = "Run"),
    "deprecated"
  )
})

test_that("plot_ef uses correct y-axis label for different ef_metrics", {
  make_ef_data <- function(metric) {
    d <- data.frame(
      date = seq(Sys.Date() - 9, Sys.Date(), by = "day"),
      ef_value = runif(10, 0.01, 0.03),
      activity_type = "Run"
    )
    class(d) <- c("athlytics_ef", class(d))
    attr(d, "params") <- list(ef_metric = metric, activity_type = "Run")
    d
  }

  p_speed <- plot_ef(make_ef_data("speed_hr"), add_trend_line = FALSE)
  expect_true(grepl("Speed", p_speed$labels$y))

  p_power <- plot_ef(make_ef_data("power_hr"), add_trend_line = FALSE)
  expect_true(grepl("Power", p_power$labels$y))

  p_gap <- plot_ef(make_ef_data("gap_hr"), add_trend_line = FALSE)
  expect_true(grepl("GAP", p_gap$labels$y))

  # Unknown metric falls back to generic label
  p_unknown <- plot_ef(make_ef_data("unknown"), add_trend_line = FALSE)
  expect_true(grepl("Efficiency Factor", p_unknown$labels$y))
})

test_that("plot_ef handles custom title, subtitle, and smooth_per_activity_type", {
  data(sample_ef)

  p_custom <- plot_ef(sample_ef, title = "My Title", subtitle = "My Sub", add_trend_line = FALSE)
  expect_equal(p_custom$labels$title, "My Title")
  expect_equal(p_custom$labels$subtitle, "My Sub")

  p_per_type <- plot_ef(sample_ef, smooth_per_activity_type = TRUE)
  geoms <- vapply(p_per_type$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSmooth" %in% geoms)
})

test_that("plot_ef group_var path works", {
  d <- data.frame(
    date = rep(seq(Sys.Date() - 9, Sys.Date(), by = "day"), 2),
    ef_value = runif(20, 0.01, 0.03),
    activity_type = "Run",
    athlete_id = rep(c("A", "B"), each = 10)
  )
  class(d) <- c("athlytics_ef", class(d))
  attr(d, "params") <- list(ef_metric = "speed_hr", activity_type = "Run")

  # group_var with smooth_per_activity_type should warn
  expect_warning(
    p <- plot_ef(d, group_var = "athlete_id", smooth_per_activity_type = TRUE),
    "smooth_per_activity_type.*ignored"
  )

  # group_var without warning
  p2 <- plot_ef(d, group_var = "athlete_id", add_trend_line = TRUE)
  geoms <- vapply(p2$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSmooth" %in% geoms)

  # Custom group colors
  p3 <- plot_ef(d, group_var = "athlete_id",
    group_colors = c(A = "red", B = "blue"), add_trend_line = FALSE)
  expect_true(length(p3$layers) >= 1)
})

test_that("plot_ef handles data without params attribute", {
  d <- data.frame(
    date = seq(Sys.Date() - 9, Sys.Date(), by = "day"),
    ef_value = runif(10, 0.01, 0.03),
    activity_type = "Run"
  )
  # No athlytics_ef class, no params attribute
  p <- plot_ef(d, add_trend_line = FALSE)
  expect_true(grepl("Unknown Metric", p$labels$subtitle))
})

# ============================================================
# EF from Stream - Verify Steady-State Detection
# ============================================================

test_that("calculate_ef_from_stream detects steady state and produces correct EF", {
  # Create perfectly steady stream data
  set.seed(123)
  n <- 3600 # 1 hour of data at 1Hz
  steady_stream <- data.frame(
    time = 0:(n - 1),
    heartrate = rep(150, n) + rnorm(n, 0, 1),
    distance = cumsum(rep(3.0, n)),
    velocity_smooth = rep(3.0, n) + rnorm(n, 0, 0.01),
    stringsAsFactors = FALSE
  )

  result <- calculate_ef_from_stream(
    stream_data = steady_stream,
    activity_date = as.Date("2024-01-01"),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.8,
    quality_control = "off"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  # Should have status "ok" for steady data
  expect_equal(result$status, "ok",
    info = "Steady-state data should produce 'ok' status"
  )

  # EF should be close to 3.0/150 = 0.02
  if (!is.na(result$ef_value)) {
    expect_true(abs(result$ef_value - 0.02) < 0.005,
      info = sprintf("EF from steady stream should be ~0.02, got %.4f", result$ef_value)
    )
  }
})

test_that("calculate_ef_from_stream handles heart_rate column renaming", {
  set.seed(456)
  n <- 3600
  stream_with_heart_rate <- data.frame(
    time = 0:(n - 1),
    heart_rate = rep(150, n) + rnorm(n, 0, 1), # "heart_rate" not "heartrate"
    velocity_smooth = rep(3.0, n) + rnorm(n, 0, 0.01),
    distance = cumsum(rep(3.0, n)),
    stringsAsFactors = FALSE
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_with_heart_rate,
    activity_date = as.Date("2024-01-01"),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.8,
    quality_control = "off"
  )

  expect_s3_class(result, "data.frame")
  # Should not fail due to missing "heartrate" column
  expect_true(result$status %in% c("ok", "non_steady"),
    info = sprintf("Stream with 'heart_rate' column should be handled, got status: %s", result$status)
  )
})

test_that("calculate_ef_from_stream handles power column renaming", {
  set.seed(789)
  n <- 3600
  stream_with_power <- data.frame(
    time = 0:(n - 1),
    heartrate = rep(150, n) + rnorm(n, 0, 1),
    power = rep(200, n) + rnorm(n, 0, 2), # "power" not "watts"
    stringsAsFactors = FALSE
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_with_power,
    activity_date = as.Date("2024-01-01"),
    act_type = "Ride",
    ef_metric = "power_hr",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.15,
    min_hr_coverage = 0.8,
    quality_control = "off"
  )

  expect_s3_class(result, "data.frame")
  # Should not fail due to missing "watts" column
  expect_true(result$status %in% c("ok", "non_steady", "too_short", "insufficient_valid_data"),
    info = sprintf("Stream with 'power' column should be renamed to 'watts', got status: %s", result$status)
  )
})

# ============================================================
# Advanced Edge Cases (merged from test-calculate-ef-advanced.R)
# ============================================================

test_that("calculate_ef handles different steady state parameters", {
  activities <- load_extdata_activities()
  win <- extdata_window(activities)

  result_strict <- calculate_ef(activities,
    activity_type = "Run", ef_metric = "speed_hr",
    min_steady_minutes = 30, steady_cv_threshold = 0.05,
    min_hr_coverage = 0.95, quality_control = "off",
    start_date = win$start_date, end_date = win$end_date
  )
  expect_s3_class(result_strict, "data.frame")

  result_lenient <- calculate_ef(activities,
    activity_type = "Run", ef_metric = "speed_hr",
    min_steady_minutes = 10, steady_cv_threshold = 0.2,
    min_hr_coverage = 0.5, quality_control = "off",
    start_date = win$start_date, end_date = win$end_date
  )
  expect_s3_class(result_lenient, "data.frame")
})

test_that("calculate_ef quality control modes handle borderline HR", {
  borderline <- data.frame(
    date = Sys.Date(), type = "Run",
    moving_time = 2400, distance = 8000,
    average_heartrate = 50, average_watts = 0,
    weighted_average_watts = 0, filename = NA,
    stringsAsFactors = FALSE
  )

  for (mode in c("filter", "flag", "off")) {
    result <- calculate_ef(borderline, quality_control = mode)
    expect_s3_class(result, "data.frame")
  }
})

test_that("calculate_ef filters by date range and activity type", {
  activities <- load_extdata_activities()
  win <- extdata_window(activities)
  mid <- win$start_date + as.integer(
    difftime(win$end_date, win$start_date, units = "days")
  ) %/% 2

  result <- calculate_ef(activities,
    start_date = mid, end_date = win$end_date,
    quality_control = "off"
  )
  expect_s3_class(result, "data.frame")

  expect_error(
    calculate_ef(activities,
      start_date = win$end_date + 100,
      end_date = win$end_date + 200,
      quality_control = "off"
    ),
    "No activities found"
  )

  expect_error(
    calculate_ef(activities,
      activity_type = "Hike", quality_control = "off",
      start_date = win$start_date, end_date = win$end_date
    ),
    "No activities found"
  )
})

test_that("calculate_ef handles very short activity", {
  short <- data.frame(
    date = Sys.Date(), type = "Run",
    moving_time = 300, distance = 1000,
    average_heartrate = 150, average_watts = 0,
    weighted_average_watts = 0, filename = NA,
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(calculate_ef(short,
    activity_type = "Run", ef_metric = "speed_hr",
    min_steady_minutes = 10, quality_control = "off"
  ))
  expect_s3_class(result, "data.frame")
})

test_that("calculate_ef handles zero and NA data gracefully", {
  zero_data <- data.frame(
    date = Sys.Date(), type = "Run",
    moving_time = 0, distance = 0,
    average_heartrate = 0, average_watts = 0,
    weighted_average_watts = 0, filename = NA,
    stringsAsFactors = FALSE
  )
  result <- suppressWarnings(calculate_ef(zero_data, quality_control = "off"))
  expect_s3_class(result, "data.frame")

  na_data <- data.frame(
    date = Sys.Date(), type = "Run",
    moving_time = NA, distance = NA,
    average_heartrate = NA, average_watts = NA,
    weighted_average_watts = NA, filename = NA,
    stringsAsFactors = FALSE
  )
  expect_error(calculate_ef(na_data, quality_control = "off"))
})

# ============================================================
# calculate_ef_from_stream Edge Cases (merged from test-ef-stream-coverage.R)
# ============================================================

test_that("calculate_ef_from_stream handles missing required columns", {
  stream_no_velocity <- data.frame(
    time = 1:100,
    heartrate = rep(150, 100)
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_no_velocity,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr"
  )
  expect_equal(result$status, "missing_velocity_data")
  expect_true(is.na(result$ef_value))

  stream_no_power <- data.frame(
    time = 1:100,
    heartrate = rep(140, 100)
  )

  result2 <- calculate_ef_from_stream(
    stream_data = stream_no_power,
    activity_date = Sys.Date(),
    act_type = "Ride",
    ef_metric = "power_hr"
  )
  expect_equal(result2$status, "missing_power_data")
  expect_true(is.na(result2$ef_value))

  stream_no_hr <- data.frame(
    time = 1:100,
    distance = 1:100
  )

  result3 <- calculate_ef_from_stream(
    stream_data = stream_no_hr,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr"
  )
  expect_equal(result3$status, "missing_hr_data")
  expect_true(is.na(result3$ef_value))
})

test_that("calculate_ef_from_stream handles insufficient data points", {
  small_stream <- data.frame(
    time = 1:50,
    heartrate = rep(150, 50),
    distance = 1:50
  )

  result <- calculate_ef_from_stream(
    stream_data = small_stream,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr"
  )

  expect_equal(result$status, "insufficient_data_points")
  expect_true(is.na(result$ef_value))
})

test_that("calculate_ef_from_stream handles low HR coverage", {
  stream_low_hr <- data.frame(
    time = 1:200,
    heartrate = c(rep(150, 30), rep(NA, 170)),
    distance = 1:200 * 10
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_low_hr,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_hr_coverage = 0.7
  )

  expect_true(result$status %in% c("insufficient_hr_data", "insufficient_valid_data", "insufficient_data_points"))
  expect_true(is.na(result$ef_value))
})

test_that("calculate_ef_from_stream handles velocity from distance", {
  stream_with_distance <- data.frame(
    time = seq(0, 599, by = 1),
    heartrate = rep(150, 600),
    distance = seq(0, 3000, length.out = 600)
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_with_distance,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 5,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.7,
    quality_control = "off"
  )

  expect_s3_class(result, "data.frame")
  expect_contains(names(result), "ef_value")
})

test_that("calculate_ef_from_stream handles quality control filtering", {
  stream_bad_values <- data.frame(
    time = seq(0, 599, by = 1),
    heartrate = c(rep(150, 300), rep(250, 300)),
    velocity_smooth = c(rep(5, 300), rep(20, 300))
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_bad_values,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 5,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.7,
    quality_control = "filter"
  )

  expect_s3_class(result, "data.frame")
})

test_that("calculate_ef_from_stream handles too short duration", {
  stream_short <- data.frame(
    time = seq(0, 120, by = 1),
    heartrate = rep(150, 121),
    velocity_smooth = rep(5, 121)
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_short,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.7,
    quality_control = "off"
  )

  expect_equal(result$status, "too_short")
  expect_true(is.na(result$ef_value))
})

test_that("calculate_ef_from_stream handles non-steady activity", {
  set.seed(123)
  stream_variable <- data.frame(
    time = seq(0, 599, by = 1),
    heartrate = rep(150, 600),
    velocity_smooth = runif(600, 3, 8)
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_variable,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 5,
    steady_cv_threshold = 0.05,
    min_hr_coverage = 0.7,
    quality_control = "off"
  )

  expect_s3_class(result, "data.frame")
  expect_contains(names(result), "status")
})

test_that("calculate_ef stream data branches with extdata", {
  act <- suppressWarnings(load_local_activities(extdata_csv))
  act_files <- act[!is.na(act$filename) & nchar(act$filename) > 0, ]

  for (i in seq_len(min(3, nrow(act_files)))) {
    ef <- suppressWarnings(calculate_ef(
      act_files[i, ],
      activity_type = act_files$type[i],
      ef_metric = "speed_hr",
      export_dir = extdata_dir,
      quality_control = "filter",
      min_duration_mins = 1,
      min_steady_minutes = 1,
      steady_cv_threshold = 0.5,
      min_hr_coverage = 0.3,
      start_date = act_files$date[i] - 1,
      end_date = act_files$date[i] + 1
    ))
    expect_s3_class(ef, "data.frame")
  }
})

# ============================================================
# Regression tests — bug-fix guards grouped by version
# ============================================================

# --- gap_hr stream path (v1.0.4: must not run the power_hr formula) -----

test_that("gap_hr stream path does not require watts (regression)", {
  # Stream has velocity + HR, but no watts. Pre-fix: returned missing_power_data.
  n <- 3600
  set.seed(1)
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = rep(150, n) + rnorm(n, 0, 0.5),
    velocity_smooth = rep(3.0, n) + rnorm(n, 0, 0.01),
    distance = cumsum(rep(3.0, n))
  )

  result <- suppressMessages(calculate_ef_from_stream(
    stream_data = stream,
    activity_date = as.Date("2024-01-01"),
    act_type = "Run",
    ef_metric = "gap_hr",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.8,
    quality_control = "off"
  ))

  expect_false(identical(result$status, "missing_power_data"),
    info = "gap_hr should fall back to velocity/HR, not require watts"
  )
  expect_false(is.na(result$ef_value))
  # EF should match speed_hr (velocity/HR ~= 3/150 = 0.02), NOT watts/HR
  expect_true(abs(result$ef_value - 0.02) < 0.005,
    info = sprintf("gap_hr stream EF expected ~0.02, got %.4f", result$ef_value)
  )
})

test_that("gap_hr stream path does not silently compute watts/heartrate (regression)", {
  # Stream has velocity AND watts. Pre-fix: silently ran watts/HR = 300/150 = 2.0
  # and labelled it gap_hr. Must now return ~velocity/HR = 0.02 instead.
  n <- 3600
  set.seed(2)
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = rep(150, n) + rnorm(n, 0, 0.5),
    velocity_smooth = rep(3.0, n) + rnorm(n, 0, 0.01),
    distance = cumsum(rep(3.0, n)),
    watts = rep(300, n) + rnorm(n, 0, 2)
  )

  result <- suppressMessages(calculate_ef_from_stream(
    stream_data = stream,
    activity_date = as.Date("2024-01-01"),
    act_type = "Run",
    ef_metric = "gap_hr",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.8,
    quality_control = "off"
  ))

  expect_false(is.na(result$ef_value))
  # Must be velocity/HR (~0.02), never watts/HR (~2.0)
  expect_lt(result$ef_value, 1)
  expect_true(abs(result$ef_value - 0.02) < 0.005)
})

# --- gap_hr fallback transparency (v1.0.5: explicit in result columns) --

test_that("calculate_ef_from_stream flags gap_hr fallback in ef_metric_used and status", {
  # Synthetic 30-minute 1 Hz run: constant 3 m/s pace, constant 150 bpm.
  # The stream has no GAP channel (none of our parsers emit one), so
  # ef_metric = "gap_hr" must fall back to speed/HR, and the returned row
  # must make that visible.
  set.seed(42)
  n <- 1800L
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = 150 + rnorm(n, 0, 1),
    velocity_smooth = 3 + rnorm(n, 0, 0.05),
    distance = cumsum(rep(3, n)),
    stringsAsFactors = FALSE
  )

  res <- suppressMessages(calculate_ef_from_stream(
    stream_data = stream,
    activity_date = as.Date("2025-01-01"),
    act_type = "Run",
    ef_metric = "gap_hr",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.2,
    min_hr_coverage = 0.5,
    quality_control = "off"
  ))

  expect_true("ef_metric_requested" %in% colnames(res))
  expect_true("ef_metric_used" %in% colnames(res))
  expect_equal(res$ef_metric_requested, "gap_hr")
  expect_equal(res$ef_metric_used, "speed_hr")
  expect_equal(res$status, "gap_stream_unavailable_fallback_to_speed")
  expect_true(is.finite(res$ef_value))
  expect_gt(res$ef_value, 0.015)
})

test_that("calculate_ef_from_stream leaves ef_metric_requested/used equal when no fallback", {
  set.seed(42)
  n <- 1800L
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = 150 + rnorm(n, 0, 1),
    velocity_smooth = 3 + rnorm(n, 0, 0.05),
    distance = cumsum(rep(3, n)),
    stringsAsFactors = FALSE
  )

  res <- suppressMessages(calculate_ef_from_stream(
    stream_data = stream,
    activity_date = as.Date("2025-01-01"),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.2,
    min_hr_coverage = 0.5,
    quality_control = "off"
  ))

  expect_equal(res$ef_metric_requested, "speed_hr")
  expect_equal(res$ef_metric_used, "speed_hr")
  expect_equal(res$status, "ok")
})

# --- Contiguous-block steady-state detection (v1.0.5) --------------------

test_that("calculate_ef_from_stream rejects scattered steady islands shorter than min_steady_minutes", {
  # Construct a 30-minute stream with 3 steady 5-minute islands separated by
  # 5-minute turbulent (high-CV) stretches. No single contiguous block is
  # >= min_steady_minutes = 15, so EF must return insufficient_steady_duration.
  set.seed(1)
  block_len <- 300L # 5 min at 1 Hz
  steady_v <- rep(3, block_len) + rnorm(block_len, 0, 0.02)
  noisy_v <- rep(3, block_len) + rnorm(block_len, 0, 1.5)

  velocity <- c(steady_v, noisy_v, steady_v, noisy_v, steady_v, noisy_v)
  hr <- rep(150, length(velocity)) + rnorm(length(velocity), 0, 1)

  stream <- data.frame(
    time = seq_along(velocity) - 1L,
    heartrate = hr,
    velocity_smooth = velocity,
    distance = cumsum(velocity),
    stringsAsFactors = FALSE
  )

  res <- suppressMessages(calculate_ef_from_stream(
    stream_data = stream,
    activity_date = as.Date("2025-01-01"),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 15,
    steady_cv_threshold = 0.08,
    min_hr_coverage = 0.5,
    quality_control = "off"
  ))

  expect_equal(res$status, "insufficient_steady_duration")
  expect_true(is.na(res$ef_value))
})

test_that("calculate_ef_from_stream accepts a contiguous steady block >= min_steady_minutes", {
  # 30-minute fully-steady run. Longest contiguous steady block is ~30 min,
  # so EF should succeed and steady_duration_minutes should be close to 30.
  set.seed(2)
  n <- 1800L
  velocity <- rep(3, n) + rnorm(n, 0, 0.02)
  hr <- rep(150, n) + rnorm(n, 0, 1)

  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = hr,
    velocity_smooth = velocity,
    distance = cumsum(velocity),
    stringsAsFactors = FALSE
  )

  res <- suppressMessages(calculate_ef_from_stream(
    stream_data = stream,
    activity_date = as.Date("2025-01-01"),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 20,
    steady_cv_threshold = 0.08,
    min_hr_coverage = 0.5,
    quality_control = "off"
  ))

  expect_equal(res$status, "ok")
  expect_gt(res$steady_duration_minutes, 19)
  expect_gte(res$n_steady_blocks, 1)
})

# --- Sampling-rate aware rolling window (v1.0.5) -------------------------

test_that("calculate_ef_from_stream reports sampling_interval_seconds for 0.5 Hz data", {
  # Half-second-resolution stream of ~30 minutes of steady 3 m/s pace.
  set.seed(3)
  n <- 3600L
  time <- seq(0, by = 0.5, length.out = n) # 0.5 Hz, total = 30 min
  velocity <- rep(3, n) + rnorm(n, 0, 0.02)
  hr <- rep(150, n) + rnorm(n, 0, 1)

  stream <- data.frame(
    time = time,
    heartrate = hr,
    velocity_smooth = velocity,
    distance = cumsum(velocity * 0.5),
    stringsAsFactors = FALSE
  )

  res <- suppressMessages(calculate_ef_from_stream(
    stream_data = stream,
    activity_date = as.Date("2025-01-01"),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 20,
    steady_cv_threshold = 0.08,
    min_hr_coverage = 0.5,
    quality_control = "off"
  ))

  expect_equal(res$status, "ok")
  expect_equal(res$sampling_interval_seconds, 0.5, tolerance = 1e-6)
})

# --- Time-weighted HR coverage (v1.0.4) ---------------------------------

test_that("calculate_ef_from_stream rejects streams with late HR dropout (regression)", {
  # 1-hour stream, but the final 45 minutes lost HR. Time-weighted coverage
  # for the 15-min valid window out of 60 min ≈ 25%. With min_hr_coverage =
  # 0.5 the stream must be rejected (pre-fix row-fraction coverage hid the gap).
  n_valid <- 15 * 60
  n_gap <- 45 * 60
  stream <- data.frame(
    time = 0:(n_valid + n_gap - 1),
    heartrate = c(rep(150, n_valid), rep(NA_real_, n_gap)),
    velocity_smooth = rep(3.0, n_valid + n_gap),
    distance = cumsum(rep(3.0, n_valid + n_gap))
  )

  result <- suppressMessages(calculate_ef_from_stream(
    stream_data = stream,
    activity_date = as.Date("2024-01-01"),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_hr_coverage = 0.5,
    min_steady_minutes = 5,
    steady_cv_threshold = 0.1,
    quality_control = "off"
  ))

  expect_identical(result$status, "insufficient_hr_data")
  expect_true(is.na(result$ef_value))
  expect_gt(result$hr_coverage, 0.2)
  expect_lt(result$hr_coverage, 0.3)
})

# --- quality_score / hr_coverage propagation (v1.0.4) -------------------

test_that("calculate_ef_from_stream ok-path returns quality_score and hr_coverage (regression)", {
  set.seed(201)
  n <- 3600
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = rep(150, n) + rnorm(n, 0, 0.5),
    velocity_smooth = rep(3.0, n) + rnorm(n, 0, 0.01),
    distance = cumsum(rep(3.0, n))
  )

  result <- suppressMessages(calculate_ef_from_stream(
    stream_data = stream,
    activity_date = as.Date("2024-01-01"),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.8,
    quality_control = "filter"
  ))

  expect_identical(result$status, "ok")
  expect_true(all(c("quality_score", "hr_coverage") %in% colnames(result)))
  expect_true(is.finite(result$quality_score))
  expect_true(is.finite(result$hr_coverage))
  expect_gte(result$quality_score, 0)
  expect_lte(result$quality_score, 1)
  expect_gte(result$hr_coverage, 0)
  expect_lte(result$hr_coverage, 1)
})

# --- Legacy capitalized metric names (v1.0.4) ---------------------------

test_that("calculate_ef accepts legacy capitalized ef_metric like 'Speed_HR' (regression)", {
  end <- Sys.Date()
  start <- end - 9
  dates <- seq(start, end, by = "day")
  n <- length(dates)

  activities <- data.frame(
    id = seq_len(n),
    name = paste("Run", seq_len(n)),
    type = "Run",
    date = dates,
    start_date_local = as.POSIXct(dates),
    distance = rep(10000, n),
    moving_time = rep(3600, n),
    elapsed_time = rep(3600, n),
    average_heartrate = rep(150, n),
    average_speed = rep(2.778, n),
    filename = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )

  expect_no_error(suppressMessages(calculate_ef(
    activities_data = activities,
    activity_type = "Run",
    ef_metric = "Speed_HR",
    quality_control = "off",
    min_duration_mins = 10,
    min_steady_minutes = 10,
    start_date = start,
    end_date = end
  )))

  expect_no_error(suppressMessages(calculate_ef(
    activities_data = activities,
    activity_type = "Run",
    ef_metric = "SPEED_HR",
    quality_control = "off",
    min_duration_mins = 10,
    min_steady_minutes = 10,
    start_date = start,
    end_date = end
  )))
})
