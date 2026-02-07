# Comprehensive test for plot_exposure.R to boost coverage

test_that("plot_exposure handles pre-calculated data", {
  data("sample_exposure")

  # Test basic plotting with pre-calculated data
  p1 <- plot_exposure(data = sample_exposure)
  expect_s3_class(p1, "ggplot")
  expect_true(length(p1$layers) >= 1)
  expect_equal(p1$labels$x, "Chronic Training Load (CTL)")
  expect_equal(p1$labels$y, "Acute Training Load (ATL)")
  expect_true(grepl("Training Load Exposure", p1$labels$title))

  # Verify plot data matches input
  expect_equal(nrow(p1$data), nrow(sample_exposure))
  expect_true(all(c("atl", "ctl") %in% names(p1$data)))

  # Test with risk_zones = FALSE
  p2 <- plot_exposure(data = sample_exposure, risk_zones = FALSE)
  expect_s3_class(p2, "ggplot")
  n_layers_no_zones <- length(p2$layers)

  # Test with risk_zones = TRUE (should have more layers for zone shading)
  p3 <- plot_exposure(data = sample_exposure, risk_zones = TRUE)
  expect_s3_class(p3, "ggplot")
  expect_true(length(p3$layers) >= n_layers_no_zones)

  # Verify risk zone lines are ablines when zones enabled
  layer_geoms_3 <- sapply(p3$layers, function(l) class(l$geom)[1])
  expect_true("GeomAbline" %in% layer_geoms_3,
    info = "Risk zones should include abline layers for zone boundaries"
  )
})

test_that("plot_exposure handles data calculation from activities", {
  # Create mock activities data
  mock_activities <- data.frame(
    date = seq(Sys.Date() - 100, Sys.Date(), by = "1 day"),
    type = rep("Run", 101),
    moving_time = rep(2400, 101),
    distance = rep(8000, 101),
    average_heartrate = rep(150, 101),
    filename = rep(NA, 101),
    stringsAsFactors = FALSE
  )

  # Calculate exposure first, then plot
  exposure_result <- calculate_exposure(
    activities_data = mock_activities,
    activity_type = "Run",
    load_metric = "duration_mins"
  )
  p <- plot_exposure(data = exposure_result)
  expect_s3_class(p, "ggplot")
  expect_true(nrow(p$data) > 0)

  # Test with different parameters
  exposure_result2 <- calculate_exposure(
    activities_data = mock_activities,
    activity_type = "Run",
    load_metric = "duration_mins",
    acute_period = 5,
    chronic_period = 30,
    end_date = Sys.Date()
  )
  p2 <- plot_exposure(data = exposure_result2)
  expect_s3_class(p2, "ggplot")
  expect_true(nrow(p2$data) > 0)
})

test_that("plot_exposure handles different load metrics", {
  # Create mock activities data
  mock_activities <- data.frame(
    date = seq(Sys.Date() - 100, Sys.Date(), by = "1 day"),
    type = rep("Run", 101),
    moving_time = rep(2400, 101),
    distance = rep(8000, 101),
    average_heartrate = rep(150, 101),
    filename = rep(NA, 101),
    stringsAsFactors = FALSE
  )

  # Test with duration_mins
  exposure_duration <- calculate_exposure(
    activities_data = mock_activities,
    activity_type = "Run",
    load_metric = "duration_mins"
  )
  p_duration <- plot_exposure(data = exposure_duration)
  expect_s3_class(p_duration, "ggplot")
  expect_true(length(p_duration$layers) >= 1)

  # Test with hrss (requires HR parameters)
  exposure_hrss <- calculate_exposure(
    activities_data = mock_activities,
    activity_type = "Run",
    load_metric = "hrss",
    user_max_hr = 200,
    user_resting_hr = 50
  )
  p_hrss <- plot_exposure(data = exposure_hrss)
  expect_s3_class(p_hrss, "ggplot")
  expect_true(length(p_hrss$layers) >= 1)
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

  # Test with single data point
  single_exposure <- data.frame(
    date = Sys.Date(),
    atl = 50,
    ctl = 40,
    acwr = 1.25
  )

  p_single <- plot_exposure(data = single_exposure)
  expect_s3_class(p_single, "ggplot")
  expect_true(length(p_single$layers) >= 1)
})

test_that("plot_exposure handles missing columns gracefully", {
  # Test with missing acwr column (only date, atl, ctl required)
  exposure_no_acwr <- data.frame(
    date = c(Sys.Date(), Sys.Date() - 1),
    atl = c(50, 45),
    ctl = c(40, 38)
  )

  p_no_acwr <- plot_exposure(data = exposure_no_acwr, risk_zones = FALSE)
  expect_s3_class(p_no_acwr, "ggplot")
  expect_true(nrow(p_no_acwr$data) == 2)
})

test_that("plot_exposure handles parameter combinations", {
  data("sample_exposure")

  # Test with risk_zones and show_date_color options
  p_combo <- plot_exposure(
    data = sample_exposure,
    risk_zones = TRUE,
    show_date_color = TRUE
  )
  expect_s3_class(p_combo, "ggplot")
  expect_true(length(p_combo$layers) >= 1)

  # Verify date color scale is present when show_date_color = TRUE
  scale_names <- sapply(p_combo$scales$scales, function(s) s$aesthetics[1])
  expect_true("colour" %in% scale_names,
    info = "Date color gradient scale should be present when show_date_color = TRUE"
  )

  p_no_date <- plot_exposure(
    data = sample_exposure,
    risk_zones = FALSE,
    show_date_color = FALSE
  )
  expect_s3_class(p_no_date, "ggplot")

  # Verify custom title and subtitle work
  p_custom <- plot_exposure(
    data = sample_exposure,
    title = "My Custom Title",
    subtitle = "My Custom Subtitle"
  )
  expect_equal(p_custom$labels$title, "My Custom Title")
  expect_equal(p_custom$labels$subtitle, "My Custom Subtitle")
})
