# tests/testthat/test-internal-load.R
#
# Tests for the internal load calculation helpers that back calculate_acwr(),
# calculate_acwr_ewma() and calculate_exposure(). The public-facing
# missing_load behaviour is covered in test-acwr.R; this file focuses on the
# `compute_single_load()` value/status contract itself (v1.0.5 regression).

test_that("compute_single_load returns ok status for valid inputs", {
  ok <- Athlytics:::compute_single_load(
    load_metric = "hrss",
    duration_sec = 3600, distance_m = 10000, elapsed_sec = 3600,
    avg_hr = 150, elevation_gain = 0, np_proxy = 0,
    user_ftp = NULL, user_max_hr = 190, user_resting_hr = 50
  )
  expect_equal(ok$status, "ok")
  expect_gt(ok$value, 0)
})

test_that("compute_single_load reports missing_heart_rate for HRSS with no HR", {
  rec <- Athlytics:::compute_single_load(
    load_metric = "hrss",
    duration_sec = 3600, distance_m = 10000, elapsed_sec = 3600,
    avg_hr = NA, elevation_gain = 0, np_proxy = 0,
    user_ftp = NULL, user_max_hr = 190, user_resting_hr = 50
  )
  expect_equal(rec$status, "missing_heart_rate")
  expect_true(is.na(rec$value))
})

test_that("compute_single_load reports missing_ftp for TSS without FTP", {
  rec <- Athlytics:::compute_single_load(
    load_metric = "tss",
    duration_sec = 3600, distance_m = 10000, elapsed_sec = 3600,
    avg_hr = 150, elevation_gain = 0, np_proxy = 250,
    user_ftp = NULL, user_max_hr = NULL, user_resting_hr = NULL
  )
  expect_equal(rec$status, "missing_ftp")
  expect_true(is.na(rec$value))
})

test_that("compute_single_load reports missing fields for non-duration metrics", {
  distance_rec <- Athlytics:::compute_single_load(
    load_metric = "distance_km",
    duration_sec = 3600, distance_m = NA_real_, elapsed_sec = 3600,
    avg_hr = 150, elevation_gain = 0, np_proxy = 250,
    user_ftp = NULL, user_max_hr = NULL, user_resting_hr = NULL
  )
  expect_equal(distance_rec$status, "missing_distance")
  expect_true(is.na(distance_rec$value))

  elapsed_rec <- Athlytics:::compute_single_load(
    load_metric = "elapsed_time_mins",
    duration_sec = 3600, distance_m = 10000, elapsed_sec = NA_real_,
    avg_hr = 150, elevation_gain = 0, np_proxy = 250,
    user_ftp = NULL, user_max_hr = NULL, user_resting_hr = NULL
  )
  expect_equal(elapsed_rec$status, "missing_elapsed_time")
  expect_true(is.na(elapsed_rec$value))

  elevation_rec <- Athlytics:::compute_single_load(
    load_metric = "elevation_gain_m",
    duration_sec = 3600, distance_m = 10000, elapsed_sec = 3600,
    avg_hr = 150, elevation_gain = NA_real_, np_proxy = 250,
    user_ftp = NULL, user_max_hr = NULL, user_resting_hr = NULL
  )
  expect_equal(elevation_rec$status, "missing_elevation_gain")
  expect_true(is.na(elevation_rec$value))
})

test_that("calculate_daily_load_internal treats absent metric columns as missing", {
  activities <- data.frame(
    date = as.Date("2024-01-01"),
    moving_time = 3600,
    elapsed_time = 3600,
    average_heartrate = 150,
    stringsAsFactors = FALSE
  )

  distance_load <- Athlytics:::calculate_daily_load_internal(
    activities,
    load_metric = "distance_km"
  )
  expect_equal(distance_load$load_status, "missing_distance")
  expect_true(is.na(distance_load$load))

  elevation_load <- Athlytics:::calculate_daily_load_internal(
    activities,
    load_metric = "elevation_gain_m"
  )
  expect_equal(elevation_load$load_status, "missing_elevation_gain")
  expect_true(is.na(elevation_load$load))

  elapsed_load <- Athlytics:::calculate_daily_load_internal(
    activities[, setdiff(names(activities), "elapsed_time"), drop = FALSE],
    load_metric = "elapsed_time_mins"
  )
  expect_equal(elapsed_load$load_status, "missing_elapsed_time")
  expect_true(is.na(elapsed_load$load))
})

test_that("calculate_daily_load_internal falls back to average watts for TSS", {
  activities <- data.frame(
    date = as.Date("2024-01-01"),
    moving_time = 3600,
    distance = 40000,
    elapsed_time = 3600,
    average_heartrate = 150,
    average_watts = 250,
    weighted_average_watts = NA_real_,
    stringsAsFactors = FALSE
  )

  tss_load <- Athlytics:::calculate_daily_load_internal(
    activities,
    load_metric = "tss",
    user_ftp = 250
  )

  expect_equal(tss_load$load_status, "ok")
  expect_equal(tss_load$load, 100)
})

test_that("calculate_daily_load_internal falls back when weighted watts is zero", {
  activities <- data.frame(
    date = as.Date("2024-01-01"),
    moving_time = 3600,
    elapsed_time = 3600,
    average_watts = 250,
    weighted_average_watts = 0,
    stringsAsFactors = FALSE
  )

  tss_load <- Athlytics:::calculate_daily_load_internal(
    activities,
    load_metric = "tss",
    user_ftp = 250
  )

  expect_equal(tss_load$load_status, "ok")
  expect_equal(tss_load$load, 100)
})

test_that("compute_single_load reports missing statuses for non-finite inputs", {
  for (bad in list(Inf, -Inf, NaN)) {
    distance_rec <- Athlytics:::compute_single_load(
      load_metric = "distance_km",
      duration_sec = 3600, distance_m = bad, elapsed_sec = 3600,
      avg_hr = 150, elevation_gain = 0, np_proxy = 250,
      user_ftp = NULL, user_max_hr = NULL, user_resting_hr = NULL
    )
    expect_equal(distance_rec$status, "missing_distance")

    elapsed_rec <- Athlytics:::compute_single_load(
      load_metric = "elapsed_time_mins",
      duration_sec = 3600, distance_m = 10000, elapsed_sec = bad,
      avg_hr = 150, elevation_gain = 0, np_proxy = 250,
      user_ftp = NULL, user_max_hr = NULL, user_resting_hr = NULL
    )
    expect_equal(elapsed_rec$status, "missing_elapsed_time")

    elevation_rec <- Athlytics:::compute_single_load(
      load_metric = "elevation_gain_m",
      duration_sec = 3600, distance_m = 10000, elapsed_sec = 3600,
      avg_hr = 150, elevation_gain = bad, np_proxy = 250,
      user_ftp = NULL, user_max_hr = NULL, user_resting_hr = NULL
    )
    expect_equal(elevation_rec$status, "missing_elevation_gain")
  }
})

test_that("distance and elevation loads do not require moving_time", {
  distance_rec <- Athlytics:::compute_single_load(
    load_metric = "distance_km",
    duration_sec = NA_real_, distance_m = 10000, elapsed_sec = NA_real_,
    avg_hr = NA_real_, elevation_gain = 100, np_proxy = 0,
    user_ftp = NULL, user_max_hr = NULL, user_resting_hr = NULL
  )
  expect_equal(distance_rec$status, "ok")
  expect_equal(distance_rec$value, 10)

  elevation_rec <- Athlytics:::compute_single_load(
    load_metric = "elevation_gain_m",
    duration_sec = NA_real_, distance_m = NA_real_, elapsed_sec = NA_real_,
    avg_hr = NA_real_, elevation_gain = 100, np_proxy = 0,
    user_ftp = NULL, user_max_hr = NULL, user_resting_hr = NULL
  )
  expect_equal(elevation_rec$status, "ok")
  expect_equal(elevation_rec$value, 100)
})
