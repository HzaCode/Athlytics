# Extended tests for plot_ef to improve coverage

library(testthat)
library(Athlytics)
library(ggplot2)

create_mock_ef_data <- function(n = 30) {
  data.frame(
    date = seq(Sys.Date() - n, Sys.Date() - 1, by = "day"),
    ef_value = runif(n, 2.5, 4.5),
    activity_type = sample(c("Run", "Ride"), n, replace = TRUE),
    ef_metric = "pace_hr",
    stringsAsFactors = FALSE
  )
}

test_that("plot_ef recognizes ef_value column", {
  skip_if_not_installed("ggplot2")

  ef_data <- create_mock_ef_data(40)

  p <- plot_ef(ef_data)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ef handles single activity type", {
  skip_if_not_installed("ggplot2")

  ef_data <- create_mock_ef_data(30)
  ef_data$activity_type <- "Run"

  p <- plot_ef(ef_data)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ef handles multiple activity types", {
  skip_if_not_installed("ggplot2")

  ef_data <- create_mock_ef_data(40)
  ef_data$activity_type <- rep(c("Run", "Ride", "VirtualRun"), length.out = 40)

  p <- plot_ef(ef_data)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ef works without trend line", {
  skip_if_not_installed("ggplot2")

  ef_data <- create_mock_ef_data(25)

  p <- plot_ef(ef_data, add_trend_line = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ef works with different smoothing methods", {
  skip_if_not_installed("ggplot2")

  ef_data <- create_mock_ef_data(50)

  # Test loess
  p1 <- plot_ef(ef_data, smoothing_method = "loess")
  expect_s3_class(p1, "ggplot")

  # Test lm
  p2 <- plot_ef(ef_data, smoothing_method = "lm")
  expect_s3_class(p2, "ggplot")
})

test_that("plot_ef handles date filtering", {
  skip_if_not_installed("ggplot2")

  ef_data <- create_mock_ef_data(60)

  p <- plot_ef(
    ef_data,
    start_date = Sys.Date() - 40,
    end_date = Sys.Date() - 10
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_ef handles NA values in ef_value", {
  skip_if_not_installed("ggplot2")

  ef_data <- create_mock_ef_data(30)
  ef_data$ef_value[c(5, 10, 15)] <- NA

  p <- plot_ef(ef_data)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ef handles small datasets", {
  skip_if_not_installed("ggplot2")

  ef_data <- create_mock_ef_data(5)

  p <- plot_ef(ef_data)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ef works with pace_hr metric", {
  skip_if_not_installed("ggplot2")

  ef_data <- create_mock_ef_data(30)
  ef_data$ef_metric <- "pace_hr"

  p <- plot_ef(ef_data)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ef works with power_hr metric", {
  skip_if_not_installed("ggplot2")

  ef_data <- create_mock_ef_data(30)
  ef_data$ef_metric <- "power_hr"
  ef_data$activity_type <- "Ride"

  p <- plot_ef(ef_data)

  expect_s3_class(p, "ggplot")
})
