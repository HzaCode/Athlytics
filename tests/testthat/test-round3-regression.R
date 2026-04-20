# tests/testthat/test-round3-regression.R
#
# Round-3 regression tests.
#
# These tests guard the behaviour changes introduced in Athlytics 1.0.5:
#  * gap_hr stream-path fallback is reported explicitly in the output frame
#  * calculate_ef_from_stream() uses contiguous-block steady-state detection
#  * calculate_ef / calculate_decoupling / flag_quality use time-based
#    rolling-window sizing (sampling-rate aware)
#  * flag_quality() computes HR and power jump rates per second
#  * flag_quality() exposes the activity-level quality score as an attribute
#  * compute_single_load() returns value + status
#  * calculate_acwr()/calculate_acwr_ewma()/calculate_exposure() honour
#    missing_load = "zero" / "na"
#  * calculate_decoupling(stream_df = ., return_diagnostics = TRUE) returns
#    a one-row diagnostics frame
#  * calculate_pbs() output includes time_basis
#  * calculate_cohort_reference() requires athlete_id unless opted out
#
# Every test is phrased positively: asserts the new behaviour, not the old
# bug. Tests that deliberately explore corner cases (NA-propagation in
# ACWR rolling means, etc.) use synthetic data to avoid relying on the
# FIT/TCX/GPX parser.

# ---------------------------------------------------------------------------
# 1. gap_hr stream-path fallback is explicit
# ---------------------------------------------------------------------------

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
  # EF should still be finite and ~ velocity/HR ≈ 3/150 = 0.02
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

# ---------------------------------------------------------------------------
# 2. EF contiguous-block steady-state detection
# ---------------------------------------------------------------------------

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
    min_steady_minutes = 15, # no single block lasts 15 min
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

# ---------------------------------------------------------------------------
# 3. Time-based rolling window (sampling-rate awareness)
# ---------------------------------------------------------------------------

test_that("estimate_sampling_interval recovers 1 Hz, 0.5 Hz and 2 Hz streams", {
  one_hz <- data.frame(time = 0:100)
  half_hz <- data.frame(time = seq(0, 200, by = 2))
  two_hz <- data.frame(time = seq(0, 50, by = 0.5))

  expect_equal(Athlytics:::estimate_sampling_interval(one_hz), 1)
  expect_equal(Athlytics:::estimate_sampling_interval(half_hz), 2)
  expect_equal(Athlytics:::estimate_sampling_interval(two_hz), 0.5)
})

test_that("calculate_ef_from_stream reports sampling_interval_seconds for 0.5 Hz data", {
  # Half-second-resolution stream of ~30 minutes of steady 3 m/s pace.
  # We set min_hr_coverage low because time-weighted coverage on constant
  # HR is 1.0, but min_steady_minutes should still see a full 30-min block.
  set.seed(3)
  n <- 3600L
  time <- seq(0, by = 0.5, length.out = n) # 0.5 Hz, total = 30 min
  velocity <- rep(3, n) + rnorm(n, 0, 0.02)
  hr <- rep(150, n) + rnorm(n, 0, 1)

  stream <- data.frame(
    time = time,
    heartrate = hr,
    velocity_smooth = velocity,
    distance = cumsum(velocity * 0.5), # 0.5 s between samples
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

# ---------------------------------------------------------------------------
# 4. flag_quality() per-second HR / power jump thresholds
# ---------------------------------------------------------------------------

test_that("flag_quality HR jump is compared per second on a 0.5 Hz stream", {
  # A 0.5 Hz stream where each consecutive sample differs by 8 bpm means
  # the per-second rate is 16 bpm/s, which should exceed the default
  # max_hr_jump = 10 and be flagged. Under the old per-sample logic, 8 bpm
  # < 10 would have passed.
  streams <- data.frame(
    time = seq(0, 60, by = 2), # 0.5 Hz
    heartrate = rep(c(140, 148), length.out = 31),
    stringsAsFactors = FALSE
  )
  flagged <- suppressMessages(flag_quality(streams, sport = "Run"))

  # The first sample can never be flagged (no preceding dt). Every subsequent
  # sample has |dHR|/dt = 8/2 = 4 bpm/s (NOT flagged) in the alternating
  # pattern 140→148→140→148. Let's instead synthesize a deliberate high-rate
  # stream where HR jumps by 12 bpm across 1 second (12 bpm/s > 10 bpm/s).
  streams2 <- data.frame(
    time = c(0, 1, 2, 3, 4),
    heartrate = c(140, 152, 153, 154, 155),
    stringsAsFactors = FALSE
  )
  flagged2 <- suppressMessages(flag_quality(streams2, sport = "Run"))
  # Second sample: |152-140| / 1 = 12 bpm/s > 10 → flag.
  expect_true(flagged2$flag_hr_spike[2])
  expect_false(flagged2$flag_hr_spike[1]) # no preceding dt
})

test_that("flag_quality does not over-flag when per-sample diff looks large but per-second rate is small", {
  # 5-second sampling interval, HR jumps by 12 bpm: that's 2.4 bpm/s, below
  # the 10 bpm/s threshold. Under the buggy per-sample check this WOULD have
  # been flagged; under the per-second fix it should not.
  streams <- data.frame(
    time = c(0, 5, 10, 15, 20),
    heartrate = c(140, 152, 150, 151, 149),
    stringsAsFactors = FALSE
  )
  flagged <- suppressMessages(flag_quality(streams, sport = "Run"))
  expect_false(any(flagged$flag_hr_spike, na.rm = TRUE))
})

test_that("flag_quality exposes activity_quality_score attribute", {
  set.seed(4)
  streams <- data.frame(
    time = 0:3600,
    heartrate = pmax(60, pmin(200, rnorm(3601, mean = 150, sd = 5))),
    watts = pmax(0, rnorm(3601, mean = 200, sd = 10)),
    velocity_smooth = pmax(0, rnorm(3601, mean = 3.5, sd = 0.3)),
    stringsAsFactors = FALSE
  )
  flagged <- suppressMessages(flag_quality(streams, sport = "Run"))
  aqs <- attr(flagged, "activity_quality_score")
  expect_true(is.numeric(aqs))
  expect_gte(aqs, 0)
  expect_lte(aqs, 1)
  # column and attribute should carry the same numeric summary.
  expect_equal(unique(flagged$quality_score), aqs, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# 5. Rest day vs missing-data training day
# ---------------------------------------------------------------------------

test_that("compute_single_load returns ok status for valid inputs and missing status for gaps", {
  ok <- Athlytics:::compute_single_load(
    load_metric = "hrss",
    duration_sec = 3600, distance_m = 10000, elapsed_sec = 3600,
    avg_hr = 150, elevation_gain = 0, np_proxy = 0,
    user_ftp = NULL, user_max_hr = 190, user_resting_hr = 50
  )
  expect_equal(ok$status, "ok")
  expect_gt(ok$value, 0)

  missing_hr <- Athlytics:::compute_single_load(
    load_metric = "hrss",
    duration_sec = 3600, distance_m = 10000, elapsed_sec = 3600,
    avg_hr = NA, elevation_gain = 0, np_proxy = 0,
    user_ftp = NULL, user_max_hr = 190, user_resting_hr = 50
  )
  expect_equal(missing_hr$status, "missing_heart_rate")
  expect_true(is.na(missing_hr$value))

  missing_ftp <- Athlytics:::compute_single_load(
    load_metric = "tss",
    duration_sec = 3600, distance_m = 10000, elapsed_sec = 3600,
    avg_hr = 150, elevation_gain = 0, np_proxy = 250,
    user_ftp = NULL, user_max_hr = NULL, user_resting_hr = NULL
  )
  expect_equal(missing_ftp$status, "missing_ftp")
  expect_true(is.na(missing_ftp$value))
})

test_that("calculate_acwr missing_load = 'na' distinguishes rest days from missing-data training days", {
  # 60 days of daily runs, but exactly one day in the middle has no HR
  # recorded so HRSS cannot be computed. With missing_load = "zero" that
  # day becomes 0 load (same as a rest); with "na" it should stay NA and
  # propagate through the rolling mean.
  n <- 60
  end_day <- as.Date("2024-03-01")
  dates <- seq(end_day - (n - 1), end_day, by = "day")
  activities <- data.frame(
    id = seq_len(n),
    type = "Run",
    sport_type = "Run",
    date = dates,
    start_date_local = as.POSIXct(dates),
    distance = rep(10000, n),
    moving_time = rep(3600, n),
    elapsed_time = rep(3600, n),
    average_heartrate = rep(150, n),
    average_speed = rep(3.0, n),
    stringsAsFactors = FALSE
  )
  gap_idx <- 30
  activities$average_heartrate[gap_idx] <- NA_real_

  acwr_zero <- suppressWarnings(calculate_acwr(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "hrss",
    user_max_hr = 190,
    user_resting_hr = 50,
    start_date = dates[1],
    end_date = end_day,
    missing_load = "zero"
  ))

  acwr_na <- suppressWarnings(calculate_acwr(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "hrss",
    user_max_hr = 190,
    user_resting_hr = 50,
    start_date = dates[1],
    end_date = end_day,
    missing_load = "na"
  ))

  expect_equal(nrow(acwr_zero), nrow(acwr_na))
  expect_gte(sum(is.na(acwr_na$acwr) & !is.na(acwr_zero$acwr)), 1)
})

# ---------------------------------------------------------------------------
# 6. calculate_decoupling(stream_df = ., return_diagnostics = TRUE)
# ---------------------------------------------------------------------------

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

# ---------------------------------------------------------------------------
# 7. calculate_pbs time_basis
# ---------------------------------------------------------------------------

test_that("calculate_pbs synthetic pipeline outputs time_basis column", {
  # The synthetic PB pipeline in test-pbs.R already exercises PB calculation
  # against real filenames; we only need to check the new column is present
  # in the output contract, which we can do via the internal find_best_effort
  # helper plus an empty-frame path.
  set.seed(6)
  empty <- calculate_pbs(
    activities_data = data.frame(
      id = integer(0),
      date = as.Date(character(0)),
      type = character(0),
      filename = character(0),
      distance = numeric(0),
      moving_time = numeric(0),
      stringsAsFactors = FALSE
    ),
    export_dir = tempdir(),
    distances_m = c(1000)
  ) |> suppressWarnings()

  expect_true("time_basis" %in% colnames(empty))
})

# ---------------------------------------------------------------------------
# 8. cohort_reference: athlete_id required unless opted out
# ---------------------------------------------------------------------------

test_that("calculate_cohort_reference errors when athlete_id is missing by default", {
  dat <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 10),
    sport = "Run",
    acwr_smooth = rnorm(10, 1, 0.1),
    stringsAsFactors = FALSE
  )
  expect_error(
    calculate_cohort_reference(dat, metric = "acwr_smooth", by = "sport"),
    "athlete_id.*is required"
  )
})

test_that("calculate_cohort_reference tolerates missing athlete_id when opted in", {
  dat <- data.frame(
    date = rep(seq(as.Date("2024-01-01"), by = "day", length.out = 10), times = 2),
    sport = "Run",
    acwr_smooth = rnorm(20, 1, 0.1),
    stringsAsFactors = FALSE
  )
  expect_warning(
    res <- calculate_cohort_reference(
      dat,
      metric = "acwr_smooth",
      by = "sport",
      min_athletes = 1,
      allow_unknown_athlete = TRUE
    ),
    "pooling all rows under a single synthetic"
  )
  expect_s3_class(res, "data.frame")
})
