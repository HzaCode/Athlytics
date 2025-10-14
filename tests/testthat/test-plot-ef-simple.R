# Simple test for plot_ef.R to boost coverage

test_that("plot_ef basic functionality", {
  data("athlytics_sample_ef")
  
  # Test basic plotting
  p1 <- plot_ef(athlytics_sample_ef)
  expect_s3_class(p1, "ggplot")
  
  # Test with add_trend_line = FALSE
  p2 <- plot_ef(athlytics_sample_ef, add_trend_line = FALSE)
  expect_s3_class(p2, "ggplot")
  
  # Test with different smoothing methods
  p3 <- plot_ef(athlytics_sample_ef, smoothing_method = "lm")
  expect_s3_class(p3, "ggplot")
})

test_that("plot_ef handles edge cases", {
  # Test with empty data
  empty_ef <- data.frame(
    date = lubridate::as_date(character(0)),
    activity_type = character(0),
    ef_value = numeric(0)
  )
  
  p_empty <- plot_ef(empty_ef)
  expect_s3_class(p_empty, "ggplot")
  
  # Test with single data point
  single_ef <- data.frame(
    date = Sys.Date(),
    activity_type = "Run",
    ef_value = 0.02
  )
  
  p_single <- plot_ef(single_ef)
  expect_s3_class(p_single, "ggplot")
})

test_that("plot_ef handles different ef_metric values", {
  data("athlytics_sample_ef")
  
  # Test with pace_hr metric
  p_pace <- plot_ef(athlytics_sample_ef, ef_metric = "pace_hr")
  expect_s3_class(p_pace, "ggplot")
  
  # Test with power_hr metric
  p_power <- plot_ef(athlytics_sample_ef, ef_metric = "power_hr")
  expect_s3_class(p_power, "ggplot")
})

test_that("plot_ef handles different activity types", {
  data("athlytics_sample_ef")
  
  # Test with different activity types
  p_run <- plot_ef(athlytics_sample_ef, activity_type = "Run")
  expect_s3_class(p_run, "ggplot")
  
  # Test with multiple activity types
  p_multi <- plot_ef(athlytics_sample_ef, activity_type = c("Run", "Ride"))
  expect_s3_class(p_multi, "ggplot")
})

test_that("plot_ef handles NA values gracefully", {
  data("athlytics_sample_ef")
  
  # Add some NA values
  ef_with_na <- athlytics_sample_ef
  ef_with_na$ef_value[1:5] <- NA
  
  p <- plot_ef(ef_with_na)
  expect_s3_class(p, "ggplot")
})
