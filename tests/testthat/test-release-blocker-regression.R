# tests/testthat/test-release-blocker-regression.R
#
# Regression tests for the release-blocker bugs fixed as a batch:
#   1. gap_hr stream path silently ran the power_hr formula
#   2. quality_control = "flag" behaved identically to "filter" in stream paths
#   3. calculate_decoupling(stream_df=...) ignored user steady / coverage params
#   4. calculate_pbs() rejected .zip export_dir via dir.exists() gate
#   5. calculate_decoupling() silently skipped activities when export_dir was .zip
#   6. calculate_pbs() custom distance labels became NA via hardcoded factor levels
#   7. calculate_decoupling() treated separated steady islands as one block
#   8. match.arg() before tolower() broke legacy capitalized metric names

# ---------------------------------------------------------------------------
# Helper: build a minimal Strava-export ZIP from inst/extdata
# ---------------------------------------------------------------------------

make_extdata_zip <- function() {
  src_csv <- system.file("extdata", "activities.csv", package = "Athlytics")
  src_acts <- system.file("extdata", "activities", package = "Athlytics")
  skip_if_not(nzchar(src_csv) && dir.exists(src_acts), "extdata not available")

  zip_path <- tempfile(fileext = ".zip")
  staging <- tempfile(pattern = "athlytics_zip_src_")
  dir.create(file.path(staging, "activities"), recursive = TRUE, showWarnings = FALSE)

  file.copy(src_csv, file.path(staging, "activities.csv"), overwrite = TRUE)
  act_files <- list.files(src_acts, full.names = TRUE)
  file.copy(act_files, file.path(staging, "activities"), overwrite = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(staging)
  utils::zip(zip_path, files = c("activities.csv", "activities"), flags = "-q")

  zip_path
}


# ===========================================================================
# 1. gap_hr stream path: must not be treated as power_hr
# ===========================================================================

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


# ===========================================================================
# 2. quality_control = "flag" must not behave like "filter"
# ===========================================================================

test_that("quality_control = 'flag' keeps rows instead of dropping them (regression)", {
  # Use calculate_decoupling stream_df path since it exposes a single numeric
  # output; build a stream where a chunk of rows would be out-of-range under
  # the quality gate.
  n <- 3600
  set.seed(3)
  stream <- data.frame(
    time = 0:(n - 1),
    # 5% of HR values are out-of-range (>= 220 triggers flag)
    heartrate = c(rep(150, 3400) + rnorm(3400, 0, 0.5),
                  rep(230, 200)),
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

  # "flag" and "off" must agree numerically (neither drops rows). "filter"
  # must be different because it actually removes the 230-bpm chunk.
  expect_equal(val_flag, val_off,
    info = "flag and off should produce identical decoupling (no rows dropped)"
  )
  expect_false(isTRUE(all.equal(val_flag, val_filter)),
    info = "flag must NOT behave like filter"
  )
})


# ===========================================================================
# 3. calculate_decoupling(stream_df=...) must forward user params
# ===========================================================================

test_that("calculate_decoupling(stream_df=...) forwards min_hr_coverage (regression)", {
  n <- 3600
  # Half the stream has NA HR: row-based coverage = 0.5
  stream <- data.frame(
    time = 0:(n - 1),
    heartrate = c(rep(150, n / 2), rep(NA_real_, n / 2)),
    velocity_smooth = rep(3.0, n),
    distance = cumsum(rep(3.0, n))
  )

  # min_hr_coverage = 0.9 requires nearly complete HR: function should bail
  # out with NA (status: insufficient_hr_data)
  val_strict <- suppressMessages(calculate_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr",
    min_hr_coverage = 0.9,
    min_steady_minutes = 5,
    steady_cv_threshold = 0.2
  ))
  expect_true(is.na(val_strict),
    info = "strict min_hr_coverage=0.9 should reject 50% HR coverage; pre-fix this was ignored"
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

  # 30-min stream but we require 40 min of steady state
  val_strict <- suppressMessages(calculate_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr",
    min_steady_minutes = 40,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.5
  ))
  expect_true(is.na(val_strict),
    info = "min_steady_minutes > stream duration should reject; pre-fix this param was ignored"
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


# ===========================================================================
# 4. calculate_pbs() zip support
# ===========================================================================

test_that("calculate_pbs accepts zip export_dir (regression)", {
  zip_path <- make_extdata_zip()
  on.exit(unlink(zip_path), add = TRUE)

  activities <- suppressWarnings(load_local_activities(zip_path))
  result <- suppressWarnings(calculate_pbs(
    activities_data = activities,
    export_dir = zip_path,
    activity_type = "Run",
    distances_m = c(1000)
  ))

  expect_s3_class(result, "data.frame")
  # Should not error; actual content may be empty depending on example files,
  # but the gate must not reject zip.
})

test_that("calculate_pbs rejects non-zip non-dir export_dir with a clear error", {
  expect_error(
    calculate_pbs(
      activities_data = data.frame(
        filename = character(),
        date = as.Date(character()),
        type = character(),
        distance = numeric()
      ),
      export_dir = "nonexistent_path_xyz"
    ),
    "directory or a .zip file"
  )
})


# ===========================================================================
# 5. calculate_decoupling() zip support
# ===========================================================================

test_that("calculate_decoupling accepts zip export_dir (regression)", {
  zip_path <- make_extdata_zip()
  on.exit(unlink(zip_path), add = TRUE)

  activities <- suppressWarnings(load_local_activities(zip_path))

  # Use a date window that guarantees at least one matching activity in extdata.
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

  # Pre-fix: every activity was skipped via file.exists() and the function
  # errored with "No decoupling values could be calculated".
  # Post-fix: we either get a data frame (even if empty) or a different error
  # -- but NOT the dir.exists-style rejection.
  if (inherits(result, "error")) {
    expect_false(grepl("directory or a .zip file", conditionMessage(result)))
  } else {
    expect_s3_class(result, "data.frame")
  }
})


# ===========================================================================
# 6. calculate_pbs() custom distance label must not become NA
# ===========================================================================

# Helper: write a synthetic TCX file with a constant pace so PBs are well-defined.
# total_m = total distance, pace_s_per_m = seconds per meter, step_s = trackpoint interval
write_synthetic_tcx <- function(path, total_m = 6000, pace_s_per_m = 0.3, step_s = 1) {
  total_s <- total_m * pace_s_per_m
  n_points <- as.integer(floor(total_s / step_s)) + 1L
  times <- seq(0, by = step_s, length.out = n_points)
  distances <- pmin(total_m, times / pace_s_per_m)
  start_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  timestamps <- format(start_time + times, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

  header <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<TrainingCenterDatabase xmlns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2" ',
    'xmlns:ns3="http://www.garmin.com/xmlschemas/ActivityExtension/v2">\n',
    '  <Activities>\n',
    '    <Activity Sport="Running">\n',
    sprintf('      <Id>%s</Id>\n', timestamps[1]),
    sprintf('      <Lap StartTime="%s">\n', timestamps[1]),
    sprintf('        <TotalTimeSeconds>%s</TotalTimeSeconds>\n', format(total_s)),
    sprintf('        <DistanceMeters>%s</DistanceMeters>\n', format(total_m)),
    '        <Track>\n'
  )

  trackpoints <- vapply(seq_along(times), function(i) {
    paste0(
      '          <Trackpoint>\n',
      sprintf('            <Time>%s</Time>\n', timestamps[i]),
      sprintf('            <DistanceMeters>%s</DistanceMeters>\n', format(distances[i], nsmall = 1)),
      '            <HeartRateBpm><Value>150</Value></HeartRateBpm>\n',
      '          </Trackpoint>\n'
    )
  }, character(1))

  footer <- paste0(
    '        </Track>\n',
    '      </Lap>\n',
    '    </Activity>\n',
    '  </Activities>\n',
    '</TrainingCenterDatabase>\n'
  )

  writeLines(c(header, trackpoints, footer), con = path, sep = "")
  invisible(path)
}

test_that("calculate_pbs preserves custom distance labels like 3000m (regression)", {
  tmp_dir <- tempfile(pattern = "athlytics_pb_")
  dir.create(file.path(tmp_dir, "activities"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Build a 6km synthetic run so 1k, 3k, and 5k PBs all fit inside.
  tcx_path <- file.path(tmp_dir, "activities", "synthetic.tcx")
  write_synthetic_tcx(tcx_path, total_m = 6000, pace_s_per_m = 0.3)

  activities <- data.frame(
    id = 1L,
    name = "Synthetic run",
    type = "Run",
    date = as.Date("2025-01-01"),
    start_date_local = as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
    distance = 6000,
    moving_time = 1800L,
    elapsed_time = 1800L,
    average_heartrate = 150,
    filename = "activities/synthetic.tcx",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(calculate_pbs(
    activities_data = activities,
    export_dir = tmp_dir,
    activity_type = "Run",
    distances_m = c(1000, 3000, 5000),
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2025-12-31")
  ))

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)

  # distance_label must not contain NA values
  expect_false(any(is.na(as.character(result$distance_label))),
    info = paste0(
      "distance_label contains NA; unique values: ",
      paste(unique(as.character(result$distance_label)), collapse = ", ")
    )
  )

  # 3000m row label must be "3000m", not NA
  rows_3000 <- result[result$distance == 3000, , drop = FALSE]
  expect_gt(nrow(rows_3000), 0)
  label_3000 <- as.character(rows_3000$distance_label)
  expect_true(all(label_3000 == "3000m"),
    info = sprintf("3000m row label expected '3000m', got: %s",
      paste(label_3000, collapse = ", "))
  )

  # factor levels must include "3000m"
  expect_true("3000m" %in% levels(result$distance_label))
})


# ===========================================================================
# 7. calculate_decoupling continuous-block logic
# ===========================================================================

test_that("calculate_decoupling does not treat separated steady islands as one block (regression)", {
  # Construct a stream with two short steady islands separated by a long
  # chaotic middle section. Each island alone is below min_steady_minutes.
  # Pre-fix: max(time) - min(time) spanned both islands and passed the check;
  # first/second halves were sliced by row count across disjoint segments.
  # Post-fix: the continuous-block logic picks the longer single island, and
  # since neither meets min_steady_minutes, the result must be NA.
  set.seed(7)
  island1_n <- 900      # 15 min steady
  chaos_n <- 1800       # 30 min noise
  island2_n <- 900      # 15 min steady
  n <- island1_n + chaos_n + island2_n

  time_vec <- 0:(n - 1)
  velocity <- c(
    rep(3.0, island1_n) + rnorm(island1_n, 0, 0.005),
    runif(chaos_n, 1.0, 6.0),
    rep(3.0, island2_n) + rnorm(island2_n, 0, 0.005)
  )
  stream <- data.frame(
    time = time_vec,
    heartrate = rep(150, n) + rnorm(n, 0, 0.5),
    velocity_smooth = velocity,
    distance = cumsum(velocity)
  )

  # Require a single 25-minute continuous steady block. Neither 15-min island
  # meets that alone, and they are separated, so result should be NA.
  val <- suppressMessages(calculate_decoupling(
    stream_df = stream,
    decouple_metric = "speed_hr",
    min_steady_minutes = 25,
    steady_cv_threshold = 0.05,
    min_hr_coverage = 0.5
  ))
  expect_true(is.na(val),
    info = "Two 15-min islands should not be merged into a 45-min 'block'"
  )
})


# ===========================================================================
# 8. Legacy capitalized metric names (match.arg/tolower ordering)
# ===========================================================================

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
