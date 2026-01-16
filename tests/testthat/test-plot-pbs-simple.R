# Simple test for plot_pbs.R to boost coverage

test_that("plot_pbs basic functionality", {
  data("sample_pbs")

  # Prepare pbs_df with proper column names
  pbs_df <- sample_pbs
  if ("date" %in% names(pbs_df) && !"activity_date" %in% names(pbs_df)) {
    names(pbs_df)[names(pbs_df) == "date"] <- "activity_date"
  }

  # Get distance_meters from the data
  req_dist_meters <- NULL
  if ("distance" %in% names(pbs_df)) {
    req_dist_meters <- unique(pbs_df$distance)
  } else if ("distance_target_m" %in% names(pbs_df)) {
    req_dist_meters <- unique(pbs_df$distance_target_m)
  }

  if (is.null(req_dist_meters) || length(req_dist_meters) == 0) {
    req_dist_meters <- c(1000, 5000, 10000)
  }

  # Test basic plotting
  p1 <- plot_pbs(pbs_df = pbs_df, distance_meters = req_dist_meters)
  expect_s3_class(p1, "ggplot")

  # Test with add_trend_line = FALSE
  p2 <- plot_pbs(pbs_df = pbs_df, distance_meters = req_dist_meters, add_trend_line = FALSE)
  expect_s3_class(p2, "ggplot")
})

test_that("plot_pbs handles edge cases", {
  # Test with NULL pbs_df and missing distance_meters
  expect_error(plot_pbs(pbs_df = NULL), "Either.*data.*or.*pbs_df.*must be provided")

  # Test with single data point
  single_pbs <- data.frame(
    activity_date = Sys.Date(),
    distance = 1000,
    elapsed_time = 300,
    is_new_pb = TRUE
  )

  p_single <- plot_pbs(pbs_df = single_pbs, distance_meters = 1000)
  expect_s3_class(p_single, "ggplot")
})

test_that("plot_pbs handles different PBS value ranges", {
  # Test with low time values
  low_pbs <- data.frame(
    activity_date = seq(Sys.Date() - 30, Sys.Date(), by = "1 day"),
    distance = rep(1000, 31),
    elapsed_time = runif(31, 180, 240),
    is_new_pb = sample(c(TRUE, FALSE), 31, replace = TRUE)
  )

  p_low <- plot_pbs(pbs_df = low_pbs, distance_meters = 1000)
  expect_s3_class(p_low, "ggplot")

  # Test with high time values
  high_pbs <- data.frame(
    activity_date = seq(Sys.Date() - 30, Sys.Date(), by = "1 day"),
    distance = rep(5000, 31),
    elapsed_time = runif(31, 1200, 1500),
    is_new_pb = sample(c(TRUE, FALSE), 31, replace = TRUE)
  )

  p_high <- plot_pbs(pbs_df = high_pbs, distance_meters = 5000)
  expect_s3_class(p_high, "ggplot")
})
