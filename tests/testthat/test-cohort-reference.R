# tests/testthat/test-cohort-reference.R

test_that("cohort_reference calculates percentiles correctly", {
  # Create multi-athlete test data
  dates <- rep(seq(as.Date("2024-01-01"), by = "day", length.out = 30), times = 10)

  cohort_data <- data.frame(
    date = dates,
    athlete_id = rep(paste0("athlete", 1:10), each = 30),
    sport = "Run",
    acwr_smooth = rnorm(300, mean = 1.0, sd = 0.2),
    stringsAsFactors = FALSE
  )

  # Calculate reference percentiles
  result <- calculate_cohort_reference(
    cohort_data,
    metric = "acwr_smooth",
    by = c("sport"),
    probs = c(0.25, 0.5, 0.75),
    min_athletes = 5
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_contains(colnames(result), c("date", "percentile", "value", "n_athletes"))

  # Check that we have 3 percentiles per date
  dates_count <- unique(result$date)
  expect_length(unique(result$percentile), 3)

  # Check that n_athletes is correct (should be 10)
  expect_equal(unique(result$n_athletes), 10)
})

test_that("cohort_reference respects min_athletes threshold", {
  # Create data with only 3 athletes
  cohort_data <- data.frame(
    date = rep(as.Date("2024-01-01"), 3),
    athlete_id = paste0("athlete", 1:3),
    sport = "Run",
    acwr_smooth = c(0.8, 1.0, 1.2),
    stringsAsFactors = FALSE
  )

  # With min_athletes = 5, should error or return empty
  expect_error(
    calculate_cohort_reference(
      cohort_data,
      metric = "acwr_smooth",
      min_athletes = 5
    ),
    "No groups have at least.*athletes"
  )

  # With min_athletes = 3, should work
  result <- calculate_cohort_reference(
    cohort_data,
    metric = "acwr_smooth",
    min_athletes = 3
  )

  expect_gt(nrow(result), 0)
})

test_that("cohort_reference handles grouping variables", {
  # Create data with multiple sports
  cohort_data <- data.frame(
    date = rep(seq(as.Date("2024-01-01"), by = "day", length.out = 10), times = 10),
    athlete_id = rep(paste0("athlete", 1:10), each = 10),
    sport = rep(c("Run", "Ride"), each = 50),
    acwr_smooth = rnorm(100, mean = 1.0, sd = 0.2),
    stringsAsFactors = FALSE
  )

  # Group by sport
  result <- calculate_cohort_reference(
    cohort_data,
    metric = "acwr_smooth",
    by = c("sport"),
    min_athletes = 3
  )

  # Should have both sports
  expect_contains(colnames(result), "sport")
  expect_setequal(unique(result$sport), c("Run", "Ride"))
})

test_that("cohort_reference validates input", {
  # Empty data
  expect_error(
    calculate_cohort_reference(data.frame()),
    "data.*empty"
  )

  # Missing required column
  bad_data <- data.frame(
    date = as.Date("2024-01-01"),
    athlete_id = "athlete1"
  )

  expect_error(
    calculate_cohort_reference(bad_data, metric = "acwr_smooth"),
    "Missing required columns"
  )
})

test_that("plot_with_reference creates valid plot", {
  # Create individual data
  individual_data <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 30),
    acwr_smooth = rnorm(30, mean = 1.1, sd = 0.15)
  )

  # Create reference data
  reference_data <- data.frame(
    date = rep(seq(as.Date("2024-01-01"), by = "day", length.out = 30), times = 5),
    percentile = rep(c("p05", "p25", "p50", "p75", "p95"), each = 30),
    value = c(
      rnorm(30, 0.6, 0.05), # p05
      rnorm(30, 0.8, 0.05), # p25
      rnorm(30, 1.0, 0.05), # p50
      rnorm(30, 1.2, 0.05), # p75
      rnorm(30, 1.4, 0.05) # p95
    ),
    n_athletes = 10
  )

  # Create plot
  p <- plot_with_reference(
    individual = individual_data,
    reference = reference_data,
    metric = "acwr_smooth"
  )

  # Check plot components (plot_with_reference not in vdiffr, validate structure)
  expect_gt(length(p$layers), 0)
})

test_that("add_reference_bands adds layers to plot", {
  # Create base plot
  plot_data <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 30),
    value = rnorm(30, 1.0, 0.1)
  )

  base_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line()

  # Create reference data
  reference_data <- data.frame(
    date = rep(seq(as.Date("2024-01-01"), by = "day", length.out = 30), times = 5),
    percentile = rep(c("p05", "p25", "p50", "p75", "p95"), each = 30),
    value = c(
      rep(0.6, 30), rep(0.8, 30), rep(1.0, 30), rep(1.2, 30), rep(1.4, 30)
    ),
    n_athletes = 10
  )

  # Add bands
  plot_with_bands <- add_reference_bands(base_plot, reference_data)

  # Check that layers were added (add_reference_bands not in vdiffr)
  expect_gt(length(plot_with_bands$layers), length(base_plot$layers))
})

test_that("cohort_reference handles missing grouping variables gracefully", {
  cohort_data <- data.frame(
    date = rep(as.Date("2024-01-01"), 10),
    athlete_id = paste0("athlete", 1:10),
    acwr_smooth = rnorm(10, 1.0, 0.2),
    stringsAsFactors = FALSE
  )

  # Request grouping by a non-existent variable
  expect_warning(
    result <- calculate_cohort_reference(
      cohort_data,
      metric = "acwr_smooth",
      by = c("sport", "nonexistent_var"), # "sport" doesn't exist
      min_athletes = 5
    ),
    "Grouping variable.*not found"
  )

  # Should still work, just without grouping
  expect_s3_class(result, "data.frame")
})


test_that("cohort_reference is deprecated but remains available", {
  dates <- rep(seq(as.Date("2024-01-01"), by = "day", length.out = 10), times = 10)
  cohort_data <- data.frame(
    date = dates,
    athlete_id = rep(paste0("athlete", 1:10), each = 10),
    sport = "Run",
    acwr_smooth = rnorm(100, mean = 1.0, sd = 0.2),
    stringsAsFactors = FALSE
  )

  expect_warning(
    out <- cohort_reference(cohort_data, metric = "acwr_smooth", by = "sport", min_athletes = 3),
    "deprecated"
  )
  expect_s3_class(out, "data.frame")
})

# ============================================================
# Regression tests — bug-fix guards grouped by version
# ============================================================

# --- Athlete-weighted percentiles (v1.0.4) ------------------------------

test_that("calculate_cohort_reference is athlete-weighted, not row-weighted (regression)", {
  # Cohort where one athlete has 10x more rows than another on the same
  # date. Under the old row-weighted behaviour that athlete dominates the
  # percentile; under the fix each athlete contributes a single
  # representative value.
  dates <- rep(as.Date("2024-06-01"), 11)
  athlete_ids <- c(rep("A", 10), "B")
  values <- c(rep(0.5, 10), 5.0) # athlete B is an outlier with ACWR = 5.0
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
  # not 0.5 (which would imply row-weighted behaviour).
  p50_row <- ref$value[ref$percentile == "p50"]
  expect_length(p50_row, 1)
  expect_gt(p50_row, 0.6)
  expect_lt(p50_row, 5.0)
})

# --- athlete_id requirement (v1.0.5) ------------------------------------

test_that("calculate_cohort_reference errors when athlete_id is missing by default (regression)", {
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

test_that("calculate_cohort_reference tolerates missing athlete_id when opted in (regression)", {
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
