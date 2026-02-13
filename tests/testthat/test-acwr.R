# tests/testthat/test-acwr.R
# ACWR Calculation and Plotting Tests
# Uses inst/extdata/activities.csv via load_extdata_activities() from helper-test-data.R

# --- Test calculate_acwr with local data ---

test_that("calculate_acwr works with activities_data parameter", {
  activities <- load_extdata_activities()
  win <- extdata_window(activities)

  acwr_result <- suppressMessages(calculate_acwr(
    activities_data = activities,
    load_metric = "duration_mins",
    activity_type = "Run",
    acute_period = 7,
    chronic_period = 28,
    start_date = win$start_date,
    end_date = win$end_date
  ))

  # Structure checks
  expect_s3_class(acwr_result, "data.frame")
  expect_contains(colnames(acwr_result), c("date", "atl", "ctl", "acwr", "acwr_smooth"))
  expect_s3_class(acwr_result$date, "Date")

  # Check that we have results
  expect_gt(nrow(acwr_result), 0)

  # Numerical checks
  expect_type(acwr_result$atl, "double")
  expect_type(acwr_result$ctl, "double")
  expect_type(acwr_result$acwr, "double")
})

test_that("calculate_acwr validates activities_data parameter", {
  # Test with non-data.frame
  expect_error(
    calculate_acwr(activities_data = "not_a_dataframe"),
    "must be a data frame"
  )

  # Test with empty or incomplete data frame
  empty_df <- data.frame()
  expect_error(
    calculate_acwr(activities_data = empty_df),
    "activity_type.*must be explicitly specified" # Now checks for activity_type first
  )
})

test_that("calculate_acwr validates period parameters", {
  activities <- load_extdata_activities()

  # acute_period must be less than chronic_period
  expect_error(
    calculate_acwr(
      activities_data = activities,
      acute_period = 28,
      chronic_period = 7
    ),
    "acute_period.*must be less than.*chronic_period"
  )
})

test_that("calculate_acwr works with different load metrics", {
  activities <- load_extdata_activities()
  win <- extdata_window(activities)

  # Test duration_mins
  acwr_duration <- suppressMessages(calculate_acwr(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "duration_mins",
    start_date = win$start_date,
    end_date = win$end_date
  ))
  expect_s3_class(acwr_duration, "data.frame")
  expect_gt(nrow(acwr_duration), 0)
  # ATL values should be non-negative (allow tiny floating-point tolerance)
  expect_true(all(acwr_duration$atl >= -1e-10, na.rm = TRUE))

  # Test distance_km
  acwr_distance <- suppressMessages(calculate_acwr(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "distance_km",
    start_date = win$start_date,
    end_date = win$end_date
  ))
  expect_s3_class(acwr_distance, "data.frame")
  expect_gt(nrow(acwr_distance), 0)
  expect_true(all(acwr_distance$atl >= -1e-10, na.rm = TRUE))

  # Test elevation
  acwr_elevation <- suppressMessages(calculate_acwr(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "elevation_gain_m",
    start_date = win$start_date,
    end_date = win$end_date
  ))
  expect_s3_class(acwr_elevation, "data.frame")
  expect_gt(nrow(acwr_elevation), 0)
})

test_that("calculate_acwr filters by activity type correctly", {
  activities <- load_extdata_activities()
  win <- extdata_window(activities)

  acwr_run <- suppressMessages(calculate_acwr(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "duration_mins",
    start_date = win$start_date,
    end_date = win$end_date
  ))

  expect_s3_class(acwr_run, "data.frame")
  expect_gt(nrow(acwr_run), 0)
})

test_that("calculate_acwr works with sample data", {
  data(sample_acwr)

  # Just check that sample data has the right structure
  expect_s3_class(sample_acwr, "data.frame")
  expect_contains(colnames(sample_acwr), c("date", "atl", "ctl", "acwr"))
})

# --- Test plot_acwr ---

test_that("plot_acwr generates correct axis labels", {
  data(sample_acwr)

  p <- plot_acwr(sample_acwr, highlight_zones = TRUE)
  expect_equal(p$labels$x, "Date")
  expect_true(grepl("ACWR", p$labels$y, ignore.case = TRUE))
})

test_that("plot_acwr validates input", {
  # Test with non-data.frame - should error
  expect_error(
    plot_acwr("not_a_dataframe"),
    "Input 'data' must be a data frame from calculate_acwr()."
  )

  # Test with missing required columns - should error
  bad_df <- data.frame(x = 1:10, y = 1:10)
  expect_error(
    plot_acwr(bad_df),
    "must be the output of calculate_acwr"
  )
})

# ============================================================
# plot_acwr Coverage - Edge Cases, Validation, Grouping
# ============================================================

test_that("plot_acwr handles empty data frame", {
  empty_df <- data.frame(date = as.Date(character()), acwr_smooth = numeric())
  expect_error(plot_acwr(empty_df), "empty")
})

test_that("plot_acwr handles legacy 'acwr' column without 'acwr_smooth'", {
  d <- data.frame(
    date = seq(Sys.Date() - 10, Sys.Date(), by = "day"),
    acwr = runif(11, 0.5, 1.5)
  )
  expect_warning(
    p <- plot_acwr(d),
    "acwr_smooth.*not found.*using.*acwr"
  )
  expect_true(length(p$layers) >= 1)
})

test_that("plot_acwr errors when both acwr_smooth and acwr are missing", {
  d <- data.frame(date = seq(Sys.Date() - 10, Sys.Date(), by = "day"), value = 1:11)
  expect_error(plot_acwr(d), "must be the output of calculate_acwr")
})

test_that("plot_acwr errors when all acwr_smooth values are NA", {
  d <- data.frame(
    date = seq(Sys.Date() - 10, Sys.Date(), by = "day"),
    acwr_smooth = rep(NA_real_, 11)
  )
  expect_error(plot_acwr(d), "No valid smoothed ACWR")
})

test_that("plot_acwr validates zone parameters", {
  data(sample_acwr)
  expect_error(plot_acwr(sample_acwr, sweet_spot_min = "bad"), "sweet_spot_min.*numeric")
  expect_error(plot_acwr(sample_acwr, sweet_spot_min = 1.5, sweet_spot_max = 0.8),
    "sweet_spot_min.*less than.*sweet_spot_max")
  expect_error(plot_acwr(sample_acwr, sweet_spot_max = 1.3, high_risk_min = 1.0),
    "high_risk_min.*greater than.*sweet_spot_max")
})

test_that("plot_acwr warns on deprecated analysis arguments", {
  data(sample_acwr)
  expect_warning(
    plot_acwr(sample_acwr, activity_type = "Run"),
    "deprecated"
  )
})

test_that("plot_acwr group_var path works", {
  d <- data.frame(
    date = rep(seq(Sys.Date() - 10, Sys.Date(), by = "day"), 2),
    acwr_smooth = runif(22, 0.5, 1.5),
    athlete_id = rep(c("A", "B"), each = 11)
  )

  p <- plot_acwr(d, group_var = "athlete_id", highlight_zones = FALSE)
  expect_true(length(p$layers) >= 1)

  # Custom group colors
  p2 <- plot_acwr(d, group_var = "athlete_id",
    group_colors = c(A = "red", B = "blue"), highlight_zones = FALSE)
  expect_true(length(p2$layers) >= 1)
})

test_that("plot_acwr handles custom title, subtitle, and no zones", {
  data(sample_acwr)
  p <- plot_acwr(sample_acwr, title = "Custom", subtitle = "Sub", highlight_zones = FALSE)
  expect_equal(p$labels$title, "Custom")
  expect_equal(p$labels$subtitle, "Sub")
})

test_that("plot_acwr subtitle auto-generates from params attribute", {
  d <- data.frame(
    date = seq(Sys.Date() - 10, Sys.Date(), by = "day"),
    acwr_smooth = runif(11, 0.8, 1.3)
  )
  attr(d, "params") <- list(load_metric = "duration_mins", activity_type = "Run",
    acute_period = 7, chronic_period = 28)
  p <- plot_acwr(d, highlight_zones = FALSE)
  expect_true(grepl("duration_mins", p$labels$subtitle))
  expect_true(grepl("7/28d", p$labels$subtitle))
})

# ============================================================
# Numerical Value Validation
# ============================================================

test_that("calculate_acwr produces correct rolling averages for constant load", {
  # Create deterministic test data: one activity per day with known load
  end_date <- Sys.Date()
  start_date <- end_date - 120
  dates <- seq(start_date, end_date, by = "day")
  n <- length(dates)

  # Fixed load of 60 minutes every day
  activities <- data.frame(
    id = seq_len(n),
    name = paste("Run", seq_len(n)),
    type = "Run",
    sport_type = "Run",
    date = dates,
    start_date_local = as.POSIXct(dates),
    distance = rep(10000, n),
    moving_time = rep(3600, n), # 60 min in seconds
    elapsed_time = rep(3600, n),
    average_heartrate = rep(150, n),
    average_speed = rep(3.0, n),
    stringsAsFactors = FALSE
  )

  result <- calculate_acwr(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "duration_mins",
    acute_period = 7,
    chronic_period = 28,
    start_date = start_date,
    end_date = end_date
  )

  # With constant daily load of 60 min:
  # ATL (7-day avg) should be 60
  # CTL (28-day avg) should be 60
  # ACWR should be ~1.0
  valid_rows <- dplyr::filter(result, !is.na(acwr))
  expect_gt(nrow(valid_rows), 0)

  # After the chronic period stabilizes, ATL and CTL should both be ~60
  late_rows <- dplyr::filter(valid_rows, date >= (end_date - 30))
  expect_true(all(abs(late_rows$atl - 60) < 1, na.rm = TRUE),
    info = "ATL should be ~60 for constant 60min daily load"
  )
  expect_true(all(abs(late_rows$ctl - 60) < 1, na.rm = TRUE),
    info = "CTL should be ~60 for constant 60min daily load"
  )
  expect_true(all(abs(late_rows$acwr - 1.0) < 0.05, na.rm = TRUE),
    info = "ACWR should be ~1.0 for constant daily load"
  )
})

test_that("calculate_acwr responds to load changes correctly", {
  # Phase 1: 28 days at 30 min/day, Phase 2: 7 days at 90 min/day
  end_date <- Sys.Date()
  start_phase1 <- end_date - 34
  dates_p1 <- seq(start_phase1, start_phase1 + 27, by = "day")
  dates_p2 <- seq(start_phase1 + 28, end_date, by = "day")
  dates_all <- c(dates_p1, dates_p2)
  n <- length(dates_all)

  activities <- data.frame(
    id = seq_len(n),
    name = paste("Run", seq_len(n)),
    type = "Run",
    sport_type = "Run",
    date = dates_all,
    start_date_local = as.POSIXct(dates_all),
    distance = rep(10000, n),
    moving_time = c(rep(1800, length(dates_p1)), rep(5400, length(dates_p2))),
    elapsed_time = c(rep(1800, length(dates_p1)), rep(5400, length(dates_p2))),
    average_heartrate = rep(150, n),
    average_speed = rep(3.0, n),
    stringsAsFactors = FALSE
  )

  result <- calculate_acwr(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "duration_mins",
    acute_period = 7,
    chronic_period = 28,
    start_date = start_phase1,
    end_date = end_date
  )

  # At the end: ATL should be ~90 (7-day avg of 90min/day)
  # ACWR should be > 1.5 after sudden load increase
  last_valid <- dplyr::filter(result, date == end_date, !is.na(acwr))

  if (nrow(last_valid) > 0) {
    expect_true(last_valid$atl > 80,
      info = sprintf("ATL should be >80 after days at 90min, got %.1f", last_valid$atl)
    )
    expect_true(last_valid$acwr > 1.5,
      info = sprintf("ACWR should be >1.5 after sudden load increase, got %.2f", last_valid$acwr)
    )
  }
})

# ============================================================
# calculate_acwr_ewma - EWMA Method & Validation
# ============================================================

test_that("calculate_acwr_ewma works with EWMA method", {
  activities <- load_extdata_activities()
  win <- extdata_window(activities)

  result <- suppressMessages(calculate_acwr_ewma(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "duration_mins",
    method = "ewma",
    half_life_acute = 3.5,
    half_life_chronic = 14,
    start_date = win$start_date,
    end_date = win$end_date
  ))

  expect_s3_class(result, "data.frame")
  expect_contains(colnames(result), c("date", "atl", "ctl", "acwr", "acwr_smooth"))
  expect_gt(nrow(result), 0)
  expect_true(all(result$atl >= 0, na.rm = TRUE))
  expect_true(all(result$ctl >= 0, na.rm = TRUE))
})

test_that("calculate_acwr_ewma CI works with EWMA method", {
  activities <- load_extdata_activities()
  win <- extdata_window(activities)

  result <- suppressMessages(calculate_acwr_ewma(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "duration_mins",
    method = "ewma",
    ci = TRUE,
    B = 20, # small B for speed
    conf_level = 0.90,
    start_date = win$start_date,
    end_date = win$end_date
  ))

  expect_contains(colnames(result), c("acwr_lower", "acwr_upper"))
  # CI lower should be <= upper where both are non-NA
  valid <- !is.na(result$acwr_lower) & !is.na(result$acwr_upper)
  if (any(valid)) {
    expect_true(all(result$acwr_lower[valid] <= result$acwr_upper[valid]))
  }
})

test_that("calculate_acwr_ewma warns when CI used with RA method", {
  activities <- load_extdata_activities()
  win <- extdata_window(activities)

  expect_warning(
    suppressMessages(calculate_acwr_ewma(
      activities_data = activities,
      activity_type = "Run",
      method = "ra",
      ci = TRUE,
      start_date = win$start_date,
      end_date = win$end_date
    )),
    "Confidence bands are only available for EWMA"
  )
})

test_that("calculate_acwr_ewma validates EWMA parameters", {
  activities <- load_extdata_activities()

  expect_error(
    calculate_acwr_ewma(activities, activity_type = "Run",
      method = "ewma", half_life_acute = -1),
    "half_life_acute.*positive"
  )
  expect_error(
    calculate_acwr_ewma(activities, activity_type = "Run",
      method = "ewma", half_life_chronic = 0),
    "half_life_chronic.*positive"
  )
  expect_error(
    calculate_acwr_ewma(activities, activity_type = "Run",
      method = "ewma", half_life_acute = 20, half_life_chronic = 10),
    "half_life_acute.*less than.*half_life_chronic"
  )
})

test_that("calculate_acwr_ewma validates RA parameters", {
  activities <- load_extdata_activities()

  expect_error(
    calculate_acwr_ewma(activities, activity_type = "Run",
      method = "ra", acute_period = -5),
    "acute_period.*positive"
  )
  expect_error(
    calculate_acwr_ewma(activities, activity_type = "Run",
      method = "ra", acute_period = 30, chronic_period = 7),
    "acute_period.*less than.*chronic_period"
  )
})

test_that("calculate_acwr_ewma validates input and load_metric", {
  expect_error(calculate_acwr_ewma(), "activities_data.*must be provided")
  expect_error(calculate_acwr_ewma(NULL), "activities_data.*must be provided")
  expect_error(calculate_acwr_ewma("string"), "must be a data frame")

  activities <- load_extdata_activities()
  expect_error(
    calculate_acwr_ewma(activities, activity_type = "Run",
      load_metric = "invalid_metric"),
    "Invalid.*load_metric"
  )
  expect_error(
    calculate_acwr_ewma(activities, activity_type = "Run",
      load_metric = "tss"),
    "user_ftp.*required"
  )
  expect_error(
    calculate_acwr_ewma(activities, activity_type = "Run",
      load_metric = "hrss"),
    "user_max_hr.*user_resting_hr.*required"
  )
})

test_that("calculate_acwr_ewma requires activity_type", {
  activities <- load_extdata_activities()
  expect_error(
    calculate_acwr_ewma(activities, activity_type = NULL),
    "activity_type.*must be explicitly specified"
  )
})

test_that("calculate_acwr_ewma validates date range", {
  activities <- load_extdata_activities()
  expect_error(
    calculate_acwr_ewma(activities, activity_type = "Run",
      start_date = Sys.Date(), end_date = Sys.Date() - 30),
    "start_date must be before end_date"
  )
})

test_that("calculate_acwr returns S3 class athlytics_acwr", {
  end_date <- Sys.Date()
  start_date <- end_date - 90
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

  result <- calculate_acwr(
    activities_data = activities,
    activity_type = "Run",
    start_date = start_date,
    end_date = end_date
  )

  expect_s3_class(result, "athlytics_acwr")
})

test_that("calculate_acwr_ewma handles single activity edge case", {
  act <- load_extdata_activities()
  single_activity <- act[act$type == "Run", ][1, , drop = FALSE]
  single_start <- single_activity$date[1] - 1
  single_end <- single_activity$date[1] + 1
  result <- calculate_acwr_ewma(
    activities_data = single_activity,
    activity_type = "Run",
    start_date = single_start,
    end_date = single_end
  )
  expect_gt(nrow(result), 0)
  expect_true(single_activity$date[1] %in% result$date)
})
