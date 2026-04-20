# tests/testthat/test-round2-regression.R
#
# Regression tests for the second batch of fixes:
#   - Time-weighted HR coverage (calculate_ef / calculate_decoupling)
#   - find_best_effort: monotonicity + interpolation + O(n) sweep
#   - ACWR rest-day vs missing-data clip
#   - Cohort reference athlete-weighted percentiles
#   - quality_score / hr_coverage propagation into EF + decoupling summaries
#   - Date-parsing explicit warning (no silent Sys.Date() fallback)
#   - flag_quality column-name aliasing and steady_cv_threshold unit unification


# ---------------------------------------------------------------------------
# 1. Time-weighted HR coverage
# ---------------------------------------------------------------------------

test_that("time_weighted_coverage integrates over time gaps (regression)", {
  # Hand-constructed stream: 9 seconds of valid HR (t=0..9) then a 991-second
  # gap with NA HR (t=1000). Row-fraction coverage would report 9/10 = 90%,
  # which hides the missing sensor entirely. Time-weighted coverage must
  # instead report the 9-second valid segment out of 1000 seconds total,
  # i.e. ~0.9 %.
  stream <- data.frame(
    time = c(0:9, 1000),
    heartrate = c(rep(150, 10), NA_real_)
  )

  cov <- Athlytics:::time_weighted_coverage(stream, "heartrate")

  expect_lt(cov, 0.05)
  expect_gt(cov, 0.0)
})

test_that("calculate_ef_from_stream rejects streams with late HR dropout (regression)", {
  # 1-hour stream, but the final 45 minutes lost HR. Row coverage = 15/60 = 25%.
  # Time coverage for the 15-minute valid window out of 60 minutes = 25%.
  # With min_hr_coverage = 0.5 the stream must be rejected.
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
  # Coverage must be reported and close to 25%
  expect_gt(result$hr_coverage, 0.2)
  expect_lt(result$hr_coverage, 0.3)
})


# ---------------------------------------------------------------------------
# 2. find_best_effort: monotonicity + interpolation + O(n)
# ---------------------------------------------------------------------------

test_that("find_best_effort interpolates target distance crossings (regression)", {
  # Stream sampled at 1 Hz, constant 3 m/s. Target = 1000 m → expected
  # elapsed time = 1000 / 3 ≈ 333.333 s. The nearest-row algorithm would
  # report 334 s (row 334 is the first where d >= 1000), introducing a
  # systematic upward bias; interpolation recovers the fractional-second
  # answer.
  d <- seq(0, 2000, by = 3)
  t <- seq(0, by = 1, length.out = length(d))
  stream <- data.frame(distance = d, time = t)

  eff <- Athlytics:::find_best_effort(stream, target_distance = 1000)

  expect_false(is.null(eff))
  expect_equal(eff$time_seconds, 1000 / 3, tolerance = 1e-6)
})

test_that("find_best_effort drops non-monotonic distance samples (regression)", {
  # Inject a GPS bounce-back that previously manufactured fake sub-second
  # 100 m segments via pure nearest-row lookup. After fix, the monotonic-
  # distance filter discards the bounce and returns the true ~33 s time.
  d_clean <- seq(0, 600, by = 3)
  t_clean <- seq(0, by = 1, length.out = length(d_clean))
  stream <- data.frame(
    distance = c(d_clean[1:50], 10, 20, 30, d_clean[51:length(d_clean)]),
    time = c(t_clean[1:50], t_clean[50] + 0.25,
      t_clean[50] + 0.5, t_clean[50] + 0.75,
      t_clean[51:length(d_clean)])
  )

  eff <- Athlytics:::find_best_effort(stream, target_distance = 100)

  expect_false(is.null(eff))
  # 100 m at 3 m/s = ~33.33 s; must not drop below 20 s due to GPS bounce.
  expect_gt(eff$time_seconds, 25)
})

test_that("find_best_effort runs in ~linear time on large streams (regression)", {
  # A 10-hour run (36000 rows) used to be O(n^2) in the number of start
  # indices. Post-fix the two-pointer sweep completes in well under a
  # second even in a cold R session.
  n <- 36000
  d <- seq(0, by = 3, length.out = n)
  t <- seq(0, by = 1, length.out = n)
  stream <- data.frame(distance = d, time = t)

  timing <- system.time({
    eff <- Athlytics:::find_best_effort(stream, target_distance = 5000)
  })

  expect_false(is.null(eff))
  # Generous bound - avoid flaky CI failures while still catching an O(n^2)
  # regression (the old algorithm took tens of seconds for n = 36000).
  expect_lt(as.numeric(timing["elapsed"]), 5)
})


# ---------------------------------------------------------------------------
# 3. ACWR rest-day vs missing-data
# ---------------------------------------------------------------------------

test_that("calculate_acwr warns when requested start_date is far before first activity (regression)", {
  end <- Sys.Date()
  first_activity <- end - 30
  start <- end - 365

  activities <- data.frame(
    id = 1:20,
    type = "Run",
    date = seq(first_activity, end, length.out = 20),
    moving_time = 3600,
    distance = 10000,
    average_heartrate = 150,
    stringsAsFactors = FALSE
  )

  expect_warning(
    suppressMessages(calculate_acwr(
      activities_data = activities,
      activity_type = "Run",
      load_metric = "distance_km",
      start_date = start,
      end_date = end
    )),
    regexp = "is.*day.*after.*start_date"
  )
})

test_that("calculate_acwr does not warn when start_date equals first_activity_date (regression)", {
  # Common benign pattern: user sets start_date to their first activity date.
  # Pre-fix the clip logic warned every time; post-fix the threshold check
  # suppresses benign 0 / 1 day gaps.
  end <- Sys.Date()
  first_activity <- end - 60
  activities <- data.frame(
    id = 1:20,
    type = "Run",
    date = seq(first_activity, end, length.out = 20),
    moving_time = 3600,
    distance = 10000,
    average_heartrate = 150,
    stringsAsFactors = FALSE
  )

  expect_no_warning(suppressMessages(calculate_acwr(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "distance_km",
    start_date = first_activity,
    end_date = end
  )))
})


# ---------------------------------------------------------------------------
# 4. Cohort reference athlete-weighted
# ---------------------------------------------------------------------------

test_that("calculate_cohort_reference is athlete-weighted, not row-weighted (regression)", {
  # Construct a cohort where one athlete has 10x more rows than another on the
  # same date. Under the old row-weighted behavior, that athlete's value
  # dominates the percentile. Under the fix, each athlete contributes a
  # single representative value.
  dates <- rep(as.Date("2024-06-01"), 11)
  athlete_ids <- c(rep("A", 10), "B")
  values <- c(rep(0.5, 10), 5.0)  # athlete B is an outlier with ACWR = 5.0
  cohort <- data.frame(
    date = dates,
    athlete_id = athlete_ids,
    sport = "Run",
    acwr_smooth = values,
    stringsAsFactors = FALSE
  )

  ref <- suppressMessages(calculate_cohort_reference(
    cohort,
    metric = "acwr_smooth",
    by = "sport",
    probs = c(0.5),
    min_athletes = 2
  ))

  # Median across two athletes (A=0.5, B=5.0) should be the average = 2.75,
  # not 0.5 (which would imply row-weighted behavior).
  p50_row <- ref$value[ref$percentile == "p50"]
  expect_length(p50_row, 1)
  expect_gt(p50_row, 0.6)
  expect_lt(p50_row, 5.0)
})


# ---------------------------------------------------------------------------
# 5. quality_score / hr_coverage propagation
# ---------------------------------------------------------------------------

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
  # result rather than the numeric-only user-facing wrapper.
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


# ---------------------------------------------------------------------------
# 6. Date-parsing explicit warning
# ---------------------------------------------------------------------------

test_that("parse_analysis_date warns on malformed input instead of silent fallback (regression)", {
  # Direct unit test for the helper.
  default <- as.Date("2024-01-01")

  expect_warning(
    parsed <- Athlytics:::parse_analysis_date(
      "2024/13/45",
      default = default,
      arg_name = "start_date"
    ),
    regexp = "Could not parse.*start_date"
  )
  expect_identical(parsed, default)

  # Valid input: no warning, correct parse.
  expect_no_warning(
    parsed_ok <- Athlytics:::parse_analysis_date(
      "2024-06-15",
      default = default,
      arg_name = "start_date"
    )
  )
  expect_identical(parsed_ok, as.Date("2024-06-15"))

  # NULL / NA: silent fallback (documented behavior).
  expect_no_warning(
    Athlytics:::parse_analysis_date(NULL, default = default, arg_name = "start_date")
  )
  expect_no_warning(
    Athlytics:::parse_analysis_date(NA, default = default, arg_name = "start_date")
  )
})


# ---------------------------------------------------------------------------
# 7. flag_quality column-name aliasing and unit unification
# ---------------------------------------------------------------------------

test_that("flag_quality accepts heart_rate / power aliases from parse_activity_file (regression)", {
  # Parse output uses heart_rate (not heartrate) and power (not watts).
  # Pre-fix, flag_quality() silently skipped all QC checks because the
  # expected columns were absent.
  stream <- data.frame(
    time = 0:99,
    heart_rate = c(rep(150, 50), 250, rep(150, 49)),  # one spike
    power = c(rep(200, 50), 1600, rep(200, 49))       # one power spike
  )

  result <- suppressMessages(flag_quality(stream, sport = "Run"))

  # Alias-applied columns must exist downstream
  expect_true("heartrate" %in% colnames(result))
  expect_true("watts" %in% colnames(result))
  # QC checks must fire on the aliased columns
  expect_true(result$flag_hr_spike[51])
  expect_true(result$flag_pw_spike[51])
})

test_that("flag_quality auto-normalizes legacy percent steady_cv_threshold with warning (regression)", {
  stream <- data.frame(
    time = 0:99,
    heartrate = rep(150, 100),
    velocity_smooth = rep(3.0, 100)
  )

  expect_warning(
    suppressMessages(flag_quality(
      stream,
      sport = "Run",
      min_steady_minutes = 1,
      steady_cv_threshold = 8 # legacy percent value
    )),
    regexp = "looks like a percentage"
  )
})

test_that("flag_quality fraction-space default produces same steady points as old percent default (regression)", {
  # Old default 8 (percent) and new default 0.08 (fraction) must select the
  # same samples as steady-state for a given stream.
  set.seed(303)
  n <- 25 * 60
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = rep(150, n),
    velocity_smooth = rnorm(n, mean = 3.5, sd = 0.1)
  )

  res_fraction <- suppressMessages(flag_quality(
    stream,
    sport = "Run",
    min_steady_minutes = 20,
    steady_cv_threshold = 0.08
  ))
  res_percent <- suppressWarnings(suppressMessages(flag_quality(
    stream,
    sport = "Run",
    min_steady_minutes = 20,
    steady_cv_threshold = 8
  )))

  expect_identical(res_fraction$is_steady_state, res_percent$is_steady_state)
})
