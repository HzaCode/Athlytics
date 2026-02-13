# tests/testthat/test-pbs.R
# PBS calculation and plotting tests

# --- plot_pbs using sample_pbs ---

test_that("plot_pbs produces expected layers with sample_pbs", {
  data("sample_pbs")

  p <- plot_pbs(data = sample_pbs)
  expect_gte(length(p$layers), 2) # At least geom_line and geom_point
})

test_that("plot_pbs handles empty data frame", {
  empty_df <- data.frame(
    activity_date = lubridate::as_date(character(0)),
    time_seconds = numeric(0),
    distance = numeric(0),
    is_pb = logical(0)
  )
  expect_error(plot_pbs(data = empty_df), "No PB data available to plot")
})

test_that("plot_pbs handles add_trend_line argument", {
  data("sample_pbs")

  p_trend <- plot_pbs(data = sample_pbs, add_trend_line = TRUE)
  p_no_trend <- plot_pbs(data = sample_pbs, add_trend_line = FALSE)

  has_smooth <- function(p) {
    sum(sapply(p$layers, function(l) inherits(l$geom, "GeomSmooth")))
  }
  expect_gt(has_smooth(p_trend), 0)
  expect_equal(has_smooth(p_no_trend), 0)
})

test_that("plot_pbs facets for multiple distances", {
  data("sample_pbs")

  if (length(unique(sample_pbs$distance)) > 1) {
    p <- plot_pbs(data = sample_pbs)
    expect_s3_class(p$facet, "FacetWrap")
    expect_equal(p$facet$params$free$y, TRUE,
      info = "Faceted PB plot should use free_y scales"
    )
  }
})

# ============================================================
# Numerical Value Validation
# ============================================================

test_that("sample_pbs has realistic pace ordering", {
  data("sample_pbs")

  pace_by_dist <- sample_pbs |>
    dplyr::group_by(distance) |>
    dplyr::summarise(
      avg_pace_per_km = mean(time_seconds / (distance / 1000)),
      .groups = "drop"
    ) |>
    dplyr::arrange(distance)

  for (i in 2:nrow(pace_by_dist)) {
    expect_true(
      pace_by_dist$avg_pace_per_km[i] > pace_by_dist$avg_pace_per_km[i - 1],
      info = sprintf(
        "Pace for %gm (%.1f s/km) should be slower than %gm (%.1f s/km)",
        pace_by_dist$distance[i], pace_by_dist$avg_pace_per_km[i],
        pace_by_dist$distance[i - 1], pace_by_dist$avg_pace_per_km[i - 1]
      )
    )
  }
})

test_that("sample_pbs individual data points satisfy pace ordering", {
  data("sample_pbs")

  multi_dist_dates <- sample_pbs |>
    dplyr::group_by(activity_date) |>
    dplyr::filter(dplyr::n_distinct(distance) > 1) |>
    dplyr::ungroup()

  if (nrow(multi_dist_dates) > 0) {
    per_point <- multi_dist_dates |>
      dplyr::mutate(pace_per_km = time_seconds / (distance / 1000)) |>
      dplyr::group_by(activity_date) |>
      dplyr::arrange(distance) |>
      dplyr::mutate(pace_ok = pace_per_km == cummax(pace_per_km)) |>
      dplyr::ungroup()

    expect_true(
      all(per_point$pace_ok),
      info = "Every data point must satisfy pace ordering"
    )
  }
})

test_that("sample_pbs PB flags are consistent with cumulative minima", {
  data("sample_pbs")

  pbs_check <- sample_pbs |>
    dplyr::group_by(distance) |>
    dplyr::arrange(activity_date) |>
    dplyr::mutate(
      expected_cummin = cummin(time_seconds),
      expected_is_pb = time_seconds == expected_cummin
    ) |>
    dplyr::ungroup()

  expect_equal(pbs_check$is_pb, pbs_check$expected_is_pb,
    info = "is_pb flags should match cumulative minimum of time_seconds"
  )
})

# ============================================================
# calculate_pbs Input Validation & Edge Cases
# ============================================================

test_that("calculate_pbs validates missing and invalid input", {
  expect_error(calculate_pbs(), "activities_data.*must be provided")
  expect_error(calculate_pbs(NULL), "activities_data.*must be provided")
  expect_error(calculate_pbs("not_a_df"), "must be a data frame")

  df_no_filename <- data.frame(date = Sys.Date(), type = "Run", distance = 5000)
  expect_error(calculate_pbs(df_no_filename, export_dir = tempdir()), "filename")
})

test_that("calculate_pbs returns empty with warning when no activities match", {
  activities <- load_extdata_activities()

  # Date range far in the future - no activities should match
  expect_warning(
    result <- calculate_pbs(activities,
      export_dir = tempdir(),
      start_date = "2099-01-01",
      end_date = "2099-01-31"
    ),
    "No activities meet the criteria"
  )
  expect_equal(nrow(result), 0)
})

test_that("calculate_pbs handles activity_type = NULL (no type filter)", {
  activities <- load_extdata_activities()

  result <- suppressWarnings(calculate_pbs(
    activities_data = activities,
    export_dir = extdata_dir,
    activity_type = NULL,
    distances_m = c(1000)
  ))
  expect_s3_class(result, "data.frame")
})

test_that("calculate_pbs handles invalid date arguments gracefully", {
  activities <- load_extdata_activities()

  result <- suppressWarnings(calculate_pbs(
    activities_data = activities,
    export_dir = extdata_dir,
    activity_type = "Run",
    end_date = "not-a-date",
    distances_m = c(1000)
  ))
  expect_s3_class(result, "data.frame")
})

test_that("calculate_pbs returns athlytics_pbs class with params", {
  activities <- load_extdata_activities()

  result <- suppressWarnings(calculate_pbs(
    activities_data = activities,
    export_dir = extdata_dir,
    activity_type = "Run",
    distances_m = c(1000, 5000)
  ))
  expect_s3_class(result, "data.frame")
  if (nrow(result) > 0) {
    expect_s3_class(result, "athlytics_pbs")
    expect_true(!is.null(attr(result, "params")))
    expect_contains(
      names(result),
      c("distance_label", "is_pb", "cumulative_pb_seconds")
    )
  }
})

# ============================================================
# find_best_effort Internal Tests
# ============================================================

test_that("find_best_effort returns NULL for insufficient data", {
  short_stream <- data.frame(
    time = as.POSIXct("2024-01-01") + 0:5,
    distance = 0:5
  )
  expect_null(Athlytics:::find_best_effort(short_stream, 1000))

  # Activity covers 500m but target is 1000m
  n <- 100
  stream_short_dist <- data.frame(
    time = as.POSIXct("2024-01-01") + 0:(n - 1),
    distance = seq(0, 500, length.out = n)
  )
  expect_null(Athlytics:::find_best_effort(stream_short_dist, 1000))
})

test_that("find_best_effort finds correct best effort in known data", {
  n <- 600
  # Constant 3 m/s for 600 seconds = 1800m total
  stream <- data.frame(
    time = as.POSIXct("2024-01-01 00:00:00") + 0:(n - 1),
    distance = seq(0, by = 3, length.out = n)
  )
  # Target 1000m at 3m/s should take ~333s
  result <- Athlytics:::find_best_effort(stream, 1000)
  expect_false(is.null(result))
  expect_true(abs(result$time_seconds - 333) < 5,
    info = sprintf("1km at 3m/s should take ~333s, got %.1f", result$time_seconds)
  )
})

# --- Integration tests with inst/extdata ---

test_that("calculate_pbs works with inst/extdata TCX files", {
  activities <- load_local_activities(extdata_csv)
  pbs_result <- suppressWarnings(calculate_pbs(
    activities_data = activities,
    export_dir = extdata_dir,
    activity_type = "Run",
    distances_m = c(1000, 3000, 5000)
  ))
  expect_s3_class(pbs_result, "data.frame")
  expect_contains(
    names(pbs_result),
    c("activity_id", "activity_date", "distance", "time_seconds", "is_pb")
  )
})

test_that("plot_pbs custom title and caption work", {
  data("sample_pbs")

  p <- plot_pbs(
    data = sample_pbs,
    title = "Custom Title",
    subtitle = "Custom Sub",
    caption = "Test caption"
  )
  expect_equal(p$labels$title, "Custom Title")
  expect_equal(p$labels$subtitle, "Custom Sub")
  expect_equal(p$labels$caption, "Test caption")
})
