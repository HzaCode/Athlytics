# tests/testthat/test-plot-snapshots.R
# Snapshot tests for plotting functions using vdiffr
# As recommended by rOpenSci reviewer

library(testthat)
library(Athlytics)

# Skip if vdiffr is not available
skip_if_not_installed("vdiffr")

test_that("plot_acwr produces consistent output", {
  vdiffr::expect_doppelganger(
    "plot_acwr_basic",
    suppressMessages(plot_acwr(data = sample_acwr))
  )
})

test_that("plot_acwr with risk zones produces consistent output", {
  vdiffr::expect_doppelganger(
    "plot_acwr_risk_zones",
    suppressMessages(plot_acwr(data = sample_acwr, highlight_zones = TRUE))
  )
})

test_that("plot_ef produces consistent output", {
  vdiffr::expect_doppelganger(
    "plot_ef_basic",
    suppressMessages(plot_ef(data = sample_ef))
  )
})

test_that("plot_ef with trend line produces consistent output", {
  vdiffr::expect_doppelganger(
    "plot_ef_trend",
    suppressMessages(plot_ef(data = sample_ef, add_trend_line = TRUE))
  )
})

test_that("plot_decoupling produces consistent output", {
  vdiffr::expect_doppelganger(
    "plot_decoupling_basic",
    suppressMessages(plot_decoupling(data = sample_decoupling))
  )
})

test_that("plot_exposure produces consistent output", {
  suppressWarnings(vdiffr::expect_doppelganger(
    "plot_exposure_basic",
    suppressMessages(plot_exposure(data = sample_exposure))
  ))
})

test_that("plot_pbs produces consistent output", {
  vdiffr::expect_doppelganger(
    "plot_pbs_basic",
    suppressMessages(plot_pbs(data = sample_pbs))
  )
})

test_that("plot_acwr_enhanced produces consistent output", {
  vdiffr::expect_doppelganger(
    "plot_acwr_enhanced_basic",
    suppressMessages(plot_acwr_enhanced(sample_acwr, show_ci = FALSE))
  )
})

test_that("plot_acwr_enhanced with zones produces consistent output", {
  vdiffr::expect_doppelganger(
    "plot_acwr_enhanced_zones",
    suppressMessages(plot_acwr_enhanced(sample_acwr, show_ci = FALSE, highlight_zones = TRUE))
  )
})

test_that("plot_acwr_comparison produces consistent output", {
  acwr_ra <- sample_acwr
  acwr_ewma <- sample_acwr
  set.seed(42)
  acwr_ewma$acwr_smooth <- acwr_ewma$acwr_smooth * runif(nrow(acwr_ewma), 0.95, 1.05)
  vdiffr::expect_doppelganger(
    "plot_acwr_comparison_basic",
    suppressMessages(plot_acwr_comparison(acwr_ra, acwr_ewma))
  )
})

# --- Extended snapshot tests for additional plot variations ---

test_that("plot_ef with smooth_per_activity_type produces consistent output", {
  vdiffr::expect_doppelganger(
    "plot_ef_smooth_per_type",
    suppressMessages(plot_ef(
      data = sample_ef, add_trend_line = TRUE,
      smooth_per_activity_type = TRUE
    ))
  )
})

test_that("plot_exposure without risk zones produces consistent output", {
  suppressWarnings(vdiffr::expect_doppelganger(
    "plot_exposure_no_zones",
    suppressMessages(plot_exposure(data = sample_exposure, risk_zones = FALSE))
  ))
})

test_that("plot_exposure without date color produces consistent output", {
  suppressWarnings(vdiffr::expect_doppelganger(
    "plot_exposure_no_date_color",
    suppressMessages(plot_exposure(data = sample_exposure, show_date_color = FALSE))
  ))
})

test_that("plot_pbs without trend line produces consistent output", {
  vdiffr::expect_doppelganger(
    "plot_pbs_no_trend",
    suppressMessages(plot_pbs(data = sample_pbs, add_trend_line = FALSE))
  )
})

test_that("plot_acwr without zones produces consistent output", {
  vdiffr::expect_doppelganger(
    "plot_acwr_no_zones",
    suppressMessages(plot_acwr(data = sample_acwr, highlight_zones = FALSE))
  )
})
