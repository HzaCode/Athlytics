# tests/testthat/test-exposure.R
# Exposure calculation and plotting tests

# --- calculate_exposure ---

test_that("calculate_exposure works with extdata activities", {
  activities <- load_extdata_activities()
  win <- extdata_window(activities)

  exposure_result <- suppressMessages(calculate_exposure(
    activities_data = activities,
    load_metric = "duration_mins",
    acute_period = 7,
    chronic_period = 28,
    end_date = win$end_date
  ))

  expect_s3_class(exposure_result, "data.frame")
  expect_contains(colnames(exposure_result), c("date", "atl", "ctl"))
  expect_gt(nrow(exposure_result), 0)
})

test_that("calculate_exposure validates input", {
  expect_error(
    calculate_exposure(activities_data = "not_a_dataframe"),
    "data frame"
  )
})

test_that("calculate_exposure works with sample data", {
  data(sample_exposure)
  expect_s3_class(sample_exposure, "data.frame")
  expect_contains(colnames(sample_exposure), "atl")
})

# --- plot_exposure (using pre-calculated ACWR data from sample data) ---

test_that("plot_exposure returns a ggplot object with sample_acwr data", {
  data(sample_acwr)
  expect_s3_class(sample_acwr, "data.frame")
  expect_contains(names(sample_acwr), c("date", "acwr", "atl", "ctl"))
  plot_exposure(sample_acwr)
})

test_that("plot_exposure handles risk_zones argument with sample_acwr", {
  data(sample_acwr)
  p_zones <- plot_exposure(sample_acwr, risk_zones = TRUE)
  p_no_zones <- plot_exposure(sample_acwr, risk_zones = FALSE)
  get_abline_layers <- function(p) sum(sapply(p$layers, function(l) inherits(l$geom, "GeomAbline")))
  expect_equal(get_abline_layers(p_zones), 3)
  expect_equal(get_abline_layers(p_no_zones), 0)
})

test_that("plot_exposure handles empty data frame input", {
  empty_df <- data.frame(
    date = as.Date(character()),
    daily_load = numeric(),
    atl = numeric(),
    ctl = numeric(),
    acwr = numeric(),
    stringsAsFactors = FALSE
  )
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
  expect_contains(names(result), c("date", "atl", "ctl"))
  expect_s3_class(result, "athlytics_exposure")

  # With constant load, ATL and CTL should converge to 60
  late_rows <- dplyr::filter(result, date >= (end_date - 30))
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

test_that("calculate_exposure does not fill unknown prehistory with zero load", {
  end_date <- as.Date("2024-03-31")
  dates <- seq(end_date - 4, end_date, by = "day")
  activities <- data.frame(
    id = seq_along(dates),
    name = paste("Run", seq_along(dates)),
    type = "Run",
    sport_type = "Run",
    date = dates,
    start_date_local = as.POSIXct(dates),
    distance = rep(10000, length(dates)),
    moving_time = rep(3600, length(dates)),
    elapsed_time = rep(3600, length(dates)),
    average_heartrate = rep(150, length(dates)),
    average_speed = rep(3.0, length(dates)),
    stringsAsFactors = FALSE
  )

  warnings <- character()
  result <- withCallingHandlers(
    calculate_exposure(
      activities_data = activities,
      activity_type = "Run",
      load_metric = "duration_mins",
      acute_period = 7,
      chronic_period = 28,
      end_date = end_date
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("Earliest activity", warnings)))
  expect_true(any(grepl("Not enough data", warnings)))
  expect_s3_class(result, "athlytics_exposure")
  expect_equal(min(result$date), min(dates))
  expect_true(all(is.na(result$ctl)))
  expect_true(all(is.na(result$acwr)))
})

test_that("calculate_exposure warns when chronic buffer is only partly observed", {
  end_date <- as.Date("2024-03-31")
  dates <- seq(as.Date("2024-02-20"), end_date, by = "day")
  activities <- data.frame(
    id = seq_along(dates),
    name = paste("Run", seq_along(dates)),
    type = "Run",
    sport_type = "Run",
    date = dates,
    start_date_local = as.POSIXct(dates),
    distance = rep(10000, length(dates)),
    moving_time = rep(3600, length(dates)),
    elapsed_time = rep(3600, length(dates)),
    average_heartrate = rep(150, length(dates)),
    average_speed = rep(3.0, length(dates)),
    stringsAsFactors = FALSE
  )

  warnings <- character()
  result <- withCallingHandlers(
    calculate_exposure(
      activities_data = activities,
      activity_type = "Run",
      load_metric = "duration_mins",
      acute_period = 7,
      chronic_period = 28,
      end_date = end_date
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("required history-buffer start", warnings)))
  expect_s3_class(result, "athlytics_exposure")
  expect_true(any(is.na(result$ctl)))
  expect_true(any(!is.na(result$ctl)))
})

test_that("calculate_exposure does not clip observed rest days at the buffer start", {
  end_date <- as.Date("2024-03-31")
  chronic_period <- 28
  analysis_start <- end_date - chronic_period + 1
  fetch_start <- analysis_start - chronic_period
  activities <- data.frame(
    id = 1:2,
    name = c("Run before buffer", "Run in window"),
    type = "Run",
    sport_type = "Run",
    date = c(fetch_start - 10, analysis_start + 16),
    start_date_local = as.POSIXct(c(fetch_start - 10, analysis_start + 16)),
    distance = c(10000, 10000),
    moving_time = c(3600, 3600),
    elapsed_time = c(3600, 3600),
    average_heartrate = c(150, 150),
    average_speed = c(3.0, 3.0),
    stringsAsFactors = FALSE
  )

  result <- expect_no_warning(suppressMessages(calculate_exposure(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "duration_mins",
    acute_period = 7,
    chronic_period = chronic_period,
    end_date = end_date
  )))

  expect_equal(min(result$date), analysis_start)
  expect_true(any(!is.na(result$ctl)))
})
