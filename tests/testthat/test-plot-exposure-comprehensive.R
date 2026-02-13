# Behavior-focused test for plot_exposure.R (visuals covered by vdiffr)

test_that("plot_exposure handles pre-calculated data", {
  data("sample_exposure")

  p1 <- plot_exposure(data = sample_exposure)
  expect_equal(nrow(p1$data), nrow(sample_exposure))
  expect_contains(names(p1$data), c("atl", "ctl"))

  p2 <- plot_exposure(data = sample_exposure, risk_zones = FALSE)
  p3 <- plot_exposure(data = sample_exposure, risk_zones = TRUE)
  geoms_no_zones <- vapply(p2$layers, function(l) class(l$geom)[1], character(1))
  geoms_zones <- vapply(p3$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomAbline" %in% geoms_no_zones)
  expect_true("GeomAbline" %in% geoms_zones)
})

test_that("plot_exposure handles data calculation from activities", {
  csv_path <- system.file("extdata", "activities.csv", package = "Athlytics")
  activities <- load_local_activities(csv_path, start_date = "2025-01-01", end_date = "2025-01-31")

  exposure_result <- calculate_exposure(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "duration_mins",
    end_date = "2025-01-31"
  )
  p <- plot_exposure(data = exposure_result)
  expect_equal(nrow(p$data), nrow(exposure_result))

  exposure_result2 <- calculate_exposure(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "duration_mins",
    acute_period = 5,
    chronic_period = 30,
    end_date = "2025-01-31"
  )
  p2 <- plot_exposure(data = exposure_result2)
  expect_equal(nrow(p2$data), nrow(exposure_result2))
})

test_that("plot_exposure handles different load metrics", {
  csv_path <- system.file("extdata", "activities.csv", package = "Athlytics")
  activities <- load_local_activities(csv_path, start_date = "2025-01-01", end_date = "2025-01-31")

  exposure_duration <- calculate_exposure(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "duration_mins",
    end_date = "2025-01-31"
  )
  p_duration <- plot_exposure(data = exposure_duration)
  expect_equal(nrow(p_duration$data), nrow(exposure_duration))

  exposure_hrss <- calculate_exposure(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "hrss",
    user_max_hr = 200,
    user_resting_hr = 50,
    end_date = "2025-01-31"
  )
  p_hrss <- plot_exposure(data = exposure_hrss)
  expect_equal(nrow(p_hrss$data), nrow(exposure_hrss))
})

test_that("plot_exposure handles edge cases", {
  # Test with empty exposure data
  empty_exposure <- data.frame(
    date = lubridate::as_date(character(0)),
    atl = numeric(0),
    ctl = numeric(0),
    acwr = numeric(0)
  )

  expect_error(
    plot_exposure(data = empty_exposure),
    regexp = "Input data frame is empty."
  )

  single_exposure <- data.frame(
    date = Sys.Date(),
    atl = 50,
    ctl = 40,
    acwr = 1.25
  )

  p_single <- plot_exposure(data = single_exposure)
  expect_equal(nrow(p_single$data), 1)
})

test_that("plot_exposure handles missing columns gracefully", {
  # Test with missing acwr column (only date, atl, ctl required)
  exposure_no_acwr <- data.frame(
    date = c(Sys.Date(), Sys.Date() - 1),
    atl = c(50, 45),
    ctl = c(40, 38)
  )

  p_no_acwr <- plot_exposure(data = exposure_no_acwr, risk_zones = FALSE)
  expect_equal(nrow(p_no_acwr$data), 2)
})

test_that("plot_exposure handles parameter combinations and labels", {
  data("sample_exposure")

  p_combo <- plot_exposure(
    data = sample_exposure,
    risk_zones = TRUE,
    show_date_color = TRUE
  )

  scale_names <- sapply(p_combo$scales$scales, function(s) s$aesthetics[1])
  expect_contains(scale_names, "colour")

  p_no_date <- plot_exposure(
    data = sample_exposure,
    risk_zones = FALSE,
    show_date_color = FALSE
  )

  p_custom <- plot_exposure(
    data = sample_exposure,
    title = "My Custom Title",
    subtitle = "My Custom Subtitle"
  )
  expect_equal(p_custom$labels$title, "My Custom Title")
  expect_equal(p_custom$labels$subtitle, "My Custom Subtitle")
})
