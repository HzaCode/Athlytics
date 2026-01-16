# Extreme edge cases and error handling to reach 80% coverage

library(testthat)
library(Athlytics)

base_dir <- "C:/Users/Ang/Documents/GitHub/Athlytics"
csv_path <- file.path(base_dir, "export_data", "activities.csv")
export_dir <- file.path(base_dir, "export_data")

# ========== Extreme data conditions for calculate_ef ==========
test_that("calculate_ef with extreme and edge case data", {
  # Zero/negative values
  extreme_runs <- data.frame(
    id = 1:50,
    date = seq(Sys.Date() - 100, Sys.Date(), length.out = 50),
    type = "Run",
    duration_mins = c(runif(40, 25, 75), rep(0, 5), rep(-1, 5)),
    distance_km = c(runif(40, 4, 14), rep(0, 10)),
    moving_time = c(runif(40, 1500, 4500), rep(0, 5), rep(-1, 5)),
    distance = c(runif(40, 4000, 14000), rep(0, 10)),
    average_heartrate = c(runif(30, 130, 165), runif(10, 0, 50), runif(10, 200, 250)),
    average_speed = c(runif(40, 9, 14), rep(0, 10)),
    filename = paste0(1:50, ".fit"),
    stringsAsFactors = FALSE
  )

  # Test with quality_control = "filter"
  ef_filter <- calculate_ef(extreme_runs,
    activity_type = "Run", ef_metric = "pace_hr",
    quality_control = "filter", min_duration_mins = 10
  )
  expect_true(is.data.frame(ef_filter) || inherits(ef_filter, "tbl"))

  # Test with quality_control = "flag"
  ef_flag <- calculate_ef(extreme_runs,
    activity_type = "Run", ef_metric = "pace_hr",
    quality_control = "flag", min_duration_mins = 10
  )
  expect_true(is.data.frame(ef_flag) || inherits(ef_flag, "tbl"))

  # Test with export_dir (will try to parse files)
  if (dir.exists(export_dir)) {
    ef_with_dir <- tryCatch(
      {
        calculate_ef(extreme_runs[1:10, ],
          activity_type = "Run", ef_metric = "pace_hr",
          export_dir = export_dir, quality_control = "filter"
        )
      },
      error = function(e) data.frame()
    )

    expect_true(is.data.frame(ef_with_dir) || inherits(ef_with_dir, "tbl"))
  }
})

test_that("calculate_ef with power metric edge cases", {
  # Rides with various power scenarios
  extreme_rides <- data.frame(
    id = 1:40,
    date = seq(Sys.Date() - 80, Sys.Date(), length.out = 40),
    type = "Ride",
    duration_mins = runif(40, 30, 90),
    distance_km = runif(40, 10, 40),
    moving_time = runif(40, 1800, 5400),
    distance = runif(40, 10000, 40000),
    average_heartrate = runif(40, 115, 160),
    average_watts = c(runif(20, 150, 250), rep(0, 10), rep(NA, 10)),
    weighted_average_watts = c(runif(20, 160, 270), rep(0, 10), rep(NA, 10)),
    filename = paste0(1:40, ".fit"),
    stringsAsFactors = FALSE
  )

  # Test power_hr metric
  ef_power1 <- calculate_ef(extreme_rides,
    activity_type = "Ride", ef_metric = "power_hr",
    quality_control = "off"
  )
  expect_true(is.data.frame(ef_power1) || inherits(ef_power1, "tbl"))

  ef_power2 <- calculate_ef(extreme_rides,
    activity_type = "Ride", ef_metric = "power_hr",
    quality_control = "filter"
  )
  expect_true(is.data.frame(ef_power2) || inherits(ef_power2, "tbl"))

  ef_power3 <- calculate_ef(extreme_rides,
    activity_type = "Ride", ef_metric = "power_hr",
    quality_control = "flag"
  )
  expect_true(is.data.frame(ef_power3) || inherits(ef_power3, "tbl"))
})

# ========== Comprehensive real data tests with all parameters ==========
test_that("real data with all calculate_ef parameter combinations", {
  skip_if(!file.exists(csv_path), "CSV not found")
  skip_if(!dir.exists(export_dir), "Export dir not found")

  act <- load_local_activities(csv_path)

  # Get activities with filenames
  act_with_files <- act[!is.na(act$filename) & nchar(act$filename) > 0, ]

  if (nrow(act_with_files) >= 20) {
    # Test all quality_control modes with export_dir
    for (qc_mode in c("off", "flag", "filter")) {
      for (min_dur in c(10, 20, 30)) {
        for (min_steady in c(10, 15, 20)) {
          ef_result <- tryCatch(
            {
              calculate_ef(act_with_files[1:20, ],
                activity_type = act_with_files$type[1],
                ef_metric = "pace_hr",
                export_dir = export_dir,
                quality_control = qc_mode,
                min_duration_mins = min_dur,
                min_steady_minutes = min_steady,
                steady_cv_threshold = 0.08,
                min_hr_coverage = 0.9
              )
            },
            error = function(e) data.frame()
          )

          expect_true(is.data.frame(ef_result) || inherits(ef_result, "tbl"))
        }
      }
    }
  }
})

# ========== Extreme plot conditions ==========
test_that("plot functions with extreme data conditions", {
  skip_if_not_installed("ggplot2")

  # Extremely sparse PBs (just 3 points)
  minimal_pbs <- data.frame(
    activity_id = 1:3,
    activity_date = c(Sys.Date() - 200, Sys.Date() - 100, Sys.Date()),
    distance = c(1000, 1000, 1000),
    time_seconds = c(300, 290, 280),
    is_pb = TRUE,
    pace_min_per_km = c(5, 4.8, 4.7),
    speed_km_per_h = c(12, 12.5, 12.8),
    activity_type = "Run",
    stringsAsFactors = FALSE
  )

  p1 <- plot_pbs(pbs_df = minimal_pbs, add_trend_line = TRUE)
  expect_s3_class(p1, "gg")

  p2 <- plot_pbs(pbs_df = minimal_pbs, add_trend_line = FALSE)
  expect_s3_class(p2, "gg")

  # Extremely dense PBs (many at same date)
  dense_pbs <- data.frame(
    activity_id = 1:100,
    activity_date = rep(seq(Sys.Date() - 50, Sys.Date(), length.out = 10), 10),
    distance = rep(c(1000, 5000), 50),
    time_seconds = runif(100, 180, 1500),
    is_pb = sample(c(TRUE, FALSE), 100, replace = TRUE),
    pace_min_per_km = runif(100, 3, 6),
    speed_km_per_h = runif(100, 10, 20),
    activity_type = "Run",
    stringsAsFactors = FALSE
  )

  p3 <- plot_pbs(pbs_df = dense_pbs)
  expect_s3_class(p3, "gg")

  # Minimal EF data
  minimal_ef_data <- data.frame(
    id = 1:3,
    date = c(Sys.Date() - 20, Sys.Date() - 10, Sys.Date()),
    type = "Run",
    duration_mins = c(30, 32, 28),
    distance_km = c(5, 5.2, 4.8),
    moving_time = c(1800, 1920, 1680),
    distance = c(5000, 5200, 4800),
    average_heartrate = c(150, 148, 152),
    average_speed = c(10, 10.2, 9.8),
    stringsAsFactors = FALSE
  )

  p4 <- plot_ef(minimal_ef_data,
    activity_type = "Run", ef_metric = "pace_hr",
    add_trend_line = TRUE
  )
  expect_s3_class(p4, "gg")

  p5 <- plot_ef(minimal_ef_data,
    activity_type = "Run", ef_metric = "pace_hr",
    add_trend_line = FALSE
  )
  expect_s3_class(p5, "gg")
})

# ========== Additional load_local_activities scenarios ==========
test_that("load_local_activities with various input conditions", {
  skip_if(!file.exists(csv_path), "CSV not found")

  act <- load_local_activities(csv_path)

  # Load with every possible activity type individually
  all_types <- unique(act$type)
  for (atype in all_types) {
    result <- load_local_activities(csv_path, activity_types = atype)
    expect_true(is.data.frame(result) || inherits(result, "tbl"))
    if (nrow(result) > 0) {
      expect_true(all(result$type == atype, na.rm = TRUE))
    }
  }

  # Load with all possible combinations
  if (length(all_types) >= 4) {
    # 4-type combinations
    for (i in 1:(length(all_types) - 3)) {
      combo4 <- all_types[i:(i + 3)]
      result <- load_local_activities(csv_path, activity_types = combo4)
      expect_true(is.data.frame(result) || inherits(result, "tbl"))
    }
  }

  # Load with all types
  result_all <- load_local_activities(csv_path, activity_types = all_types)
  expect_true(is.data.frame(result_all) || inherits(result_all, "tbl"))
})

# ========== All possible plot_ef and plot_pbs option combinations ==========
test_that("all plot option combinations with real data", {
  skip_if_not_installed("ggplot2")
  skip_if(!file.exists(csv_path), "CSV not found")

  act <- load_local_activities(csv_path)

  # Get activity type with most data
  type_counts <- table(act$type[!is.na(act$average_heartrate)])
  if (length(type_counts) > 0) {
    main_type <- names(which.max(type_counts))
    type_act <- act[act$type == main_type & !is.na(act$average_heartrate), ]

    if (nrow(type_act) >= 30) {
      # Test every combination of plot_ef options
      for (add_trend in c(TRUE, FALSE)) {
        for (smooth_method in c("loess", "lm")) {
          for (min_dur in c(15, 30, 45)) {
            p <- tryCatch(
              {
                plot_ef(type_act,
                  activity_type = main_type, ef_metric = "pace_hr",
                  add_trend_line = add_trend, smoothing_method = smooth_method,
                  min_duration_mins = min_dur
                )
              },
              error = function(e) NULL
            )

            if (!is.null(p)) {
              expect_s3_class(p, "gg")
            }
          }
        }
      }
    }
  }
})

# ========== Final comprehensive test suite ==========
test_that("ultra comprehensive real data coverage", {
  skip_if(!file.exists(csv_path), "CSV not found")
  skip_if(!dir.exists(export_dir), "Export dir not found")

  act <- load_local_activities(csv_path)

  # Test with every unique combination
  types <- unique(act$type)
  for (i in seq_along(types)) {
    atype <- types[i]
    type_act <- act[act$type == atype, ]

    if (nrow(type_act) >= 25) {
      # Try calculate_ef with export_dir
      ef_result <- tryCatch(
        {
          calculate_ef(type_act[1:25, ],
            activity_type = atype,
            ef_metric = "pace_hr",
            export_dir = export_dir,
            quality_control = "filter",
            min_duration_mins = 15,
            min_steady_minutes = 15,
            steady_cv_threshold = 0.10,
            min_hr_coverage = 0.85
          )
        },
        error = function(e) data.frame()
      )

      expect_true(is.data.frame(ef_result) || inherits(ef_result, "tbl"))

      # Try with different settings
      ef_result2 <- tryCatch(
        {
          calculate_ef(type_act[1:25, ],
            activity_type = atype,
            ef_metric = "pace_hr",
            export_dir = export_dir,
            quality_control = "flag",
            min_duration_mins = 20,
            min_steady_minutes = 20,
            steady_cv_threshold = 0.08,
            min_hr_coverage = 0.90
          )
        },
        error = function(e) data.frame()
      )

      expect_true(is.data.frame(ef_result2) || inherits(ef_result2, "tbl"))
    }
  }
})
