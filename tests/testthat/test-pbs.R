# tests/testthat/test-pbs.R

library(testthat)
library(Athlytics)
suppressWarnings(library(ggplot2)) # Explicitly load for s3_class checks if not automatically available
library(lubridate) # For seconds_to_period if used in manual_df

# Load data: sample data from package & helper mock data
data(sample_pbs)
source(test_path("helper-mockdata.R"), local = TRUE)

# --- Test plot_pbs using manually created, well-structured pbs_df ---

test_that("plot_pbs returns a ggplot object with valid pbs_df input", {
  # 1. Manually create a small, representative pbs_df
  #    This data frame should have the exact column names and types
  #    that calculate_pbs would output and plot_pbs expects.
  manual_pbs_df <- data.frame(
    activity_date = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15", "2023-01-01", "2023-01-20")),
    time_seconds = c(1200, 1180, 1190, 300, 1170), # Example times
    distance = c(5000, 5000, 5000, 1000, 5000), # Example distances
    is_pb = c(TRUE, TRUE, FALSE, TRUE, TRUE), # Example PB flags
    distance_label = factor(c("5k", "5k", "5k", "1k", "5k"), levels = c("1k", "5k")),
    activity_id = as.character(1:5),
    elapsed_time = c(1200, 1180, 1190, 300, 1170),
    moving_time = c(1200, 1180, 1190, 300, 1170),
    cumulative_pb_seconds = c(1200, 1180, 1180, 300, 1170),
    # time_period is derived by plot_pbs if not present, or by calculate_pbs
    # For a direct pbs_df input test, ensure it has what plot_pbs *uses* or can work without.
    # calculate_pbs does output time_period.
    stringsAsFactors = FALSE
  )
  # Add time_period for completeness matching calculate_pbs output
  manual_pbs_df$time_period <- lubridate::seconds_to_period(manual_pbs_df$time_seconds)


  # Ensure distance_meters argument for plot_pbs matches distances in manual_pbs_df
  test_distance_meters <- unique(manual_pbs_df$distance)

  # 2. Call plot_pbs with this manually created data
  # suppressWarnings: distance_meters is analysis arg, ignored with pre-calculated data
  p <- suppressWarnings(plot_pbs(data = manual_pbs_df, distance_meters = test_distance_meters))

  # 3. Perform assertions on the plot object 'p'
  expect_s3_class(p, "ggplot")

  # Check for expected layers if possible (e.g., points, lines)
  expect_true(length(p$layers) >= 2) # Expect at least geom_line and geom_point

  # Check labels (example)
  expect_equal(p$labels$x, "Activity Date")
  expect_equal(p$labels$y, "Best Time (MM:SS)")
  expect_equal(p$labels$title, "Personal Best Running Times Trend")
})

test_that("plot_pbs handles empty data frame input", {
  # Create an empty data frame with the correct column structure
  empty_df <- data.frame(
    activity_date = as.Date(character()),
    time_seconds = numeric(),
    distance = numeric(),
    is_pb = logical(),
    distance_label = factor(levels = c("1k", "5k")), # ensure factor levels if used
    activity_id = character(),
    elapsed_time = numeric(),
    moving_time = numeric(),
    cumulative_pb_seconds = numeric(),
    time_period = lubridate::seconds_to_period(numeric()),
    stringsAsFactors = FALSE
  )

  # plot_pbs requires distance_meters to be specified, even if pbs_df is empty,
  # because it uses it for filtering and potentially for calculate_pbs call.
  # However, if pbs_df is empty, the internal calculate_pbs won't be called.
  # The warning comes from `pbs_df[pbs_df$distance %in% distance_meters,]` if it results in empty or
  # directly from the `nrow(pbs_df) == 0` check.
  # suppressWarnings: distance_meters is analysis arg, ignored with pre-calculated data
  expect_error(
    suppressWarnings(plot_pbs(data = empty_df, distance_meters = c(1000, 5000))),
    regexp = "No PB data available to plot|pbs_df does not contain data for the specified distance_meters"
  )
})

# Test for add_trend_line argument
test_that("plot_pbs handles add_trend_line argument", {
  manual_pbs_df <- data.frame(
    activity_date = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15")),
    time_seconds = c(1200, 1180, 1190), distance = c(5000, 5000, 5000),
    is_pb = c(TRUE, TRUE, FALSE), distance_label = factor(c("5k", "5k", "5k")),
    activity_id = as.character(1:3), elapsed_time = c(1200, 1180, 1190),
    moving_time = c(1200, 1180, 1190), cumulative_pb_seconds = c(1200, 1180, 1180),
    stringsAsFactors = FALSE
  )
  manual_pbs_df$time_period <- lubridate::seconds_to_period(manual_pbs_df$time_seconds)
  test_dist_meters <- unique(manual_pbs_df$distance)

  # suppressWarnings: distance_meters is analysis arg, ignored with pre-calculated data
  p_trend <- suppressWarnings(plot_pbs(data = manual_pbs_df, distance_meters = test_dist_meters, add_trend_line = TRUE))
  p_no_trend <- suppressWarnings(plot_pbs(data = manual_pbs_df, distance_meters = test_dist_meters, add_trend_line = FALSE))

  get_smooth_layers <- function(p) sum(sapply(p$layers, function(l) inherits(l$geom, "GeomSmooth")))
  expect_equal(get_smooth_layers(p_trend), 1)
  expect_equal(get_smooth_layers(p_no_trend), 0)
})

# Optional: Test faceting if multiple distances are present
test_that("plot_pbs facets for multiple distances", {
  manual_pbs_df_multi_dist <- data.frame(
    activity_date = as.Date(c("2023-01-01", "2023-01-08", "2023-01-01")),
    time_seconds = c(1200, 1180, 300), distance = c(5000, 5000, 1000),
    is_pb = c(TRUE, TRUE, TRUE), distance_label = factor(c("5k", "5k", "1k"), levels = c("1k", "5k")),
    activity_id = as.character(1:3), elapsed_time = c(1200, 1180, 300),
    moving_time = c(1200, 1180, 300), cumulative_pb_seconds = c(1200, 1180, 300),
    stringsAsFactors = FALSE
  )
  manual_pbs_df_multi_dist$time_period <- lubridate::seconds_to_period(manual_pbs_df_multi_dist$time_seconds)
  test_dist_meters_multi <- unique(manual_pbs_df_multi_dist$distance)

  # suppressWarnings: distance_meters is analysis arg, ignored with pre-calculated data
  p_multi <- suppressWarnings(plot_pbs(data = manual_pbs_df_multi_dist, distance_meters = test_dist_meters_multi))

  # Check if faceting is applied (presence of FacetWrap class in plot object)
  is_faceted <- inherits(p_multi$facet, "FacetWrap")
  expect_true(is_faceted)
})

# ============================================================
# Numerical Value Validation
# ============================================================

test_that("sample_pbs has realistic pace ordering", {
  data("sample_pbs", package = "Athlytics")

  # Calculate average pace per km for each distance
  pace_by_dist <- sample_pbs %>%
    dplyr::group_by(distance) %>%
    dplyr::summarise(avg_pace_per_km = mean(time_seconds / (distance / 1000)), .groups = "drop") %>%
    dplyr::arrange(distance)

  # Longer distances must have slower per-km pace
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
  data("sample_pbs", package = "Athlytics")

  # On any date with multiple distances, longer distance must have slower per-km pace
  multi_dist_dates <- sample_pbs %>%
    dplyr::group_by(activity_date) %>%
    dplyr::filter(dplyr::n_distinct(distance) > 1) %>%
    dplyr::ungroup()

  if (nrow(multi_dist_dates) > 0) {
    per_point <- multi_dist_dates %>%
      dplyr::mutate(pace_per_km = time_seconds / (distance / 1000)) %>%
      dplyr::group_by(activity_date) %>%
      dplyr::arrange(distance) %>%
      dplyr::mutate(pace_ok = pace_per_km == cummax(pace_per_km)) %>%
      dplyr::ungroup()

    expect_true(
      all(per_point$pace_ok),
      info = "Every individual data point must satisfy pace ordering (longer distance = slower per-km pace)"
    )
  }
})

test_that("sample_pbs PB flags are consistent with cumulative minima", {
  data("sample_pbs", package = "Athlytics")

  # For each distance, is_pb should be TRUE only when time_seconds equals the cumulative minimum
  pbs_check <- sample_pbs %>%
    dplyr::group_by(distance) %>%
    dplyr::arrange(activity_date) %>%
    dplyr::mutate(
      expected_cummin = cummin(time_seconds),
      expected_is_pb = time_seconds == expected_cummin
    ) %>%
    dplyr::ungroup()

  expect_equal(pbs_check$is_pb, pbs_check$expected_is_pb,
    info = "is_pb flags should match cumulative minimum of time_seconds"
  )
})
