# Package index

## Data Loading

Functions for loading and parsing local Strava data

- [`load_local_activities()`](https://hzacode.github.io/Athlytics/reference/load_local_activities.md)
  : Load Activities from Local Strava Export
- [`parse_activity_file()`](https://hzacode.github.io/Athlytics/reference/parse_activity_file.md)
  : Parse Activity File (FIT, TCX, or GPX)

## Core Metrics Calculation

Calculate key physiological metrics

- [`calculate_acwr()`](https://hzacode.github.io/Athlytics/reference/calculate_acwr.md)
  : Calculate Acute:Chronic Workload Ratio (ACWR)
- [`calculate_acwr_ewma()`](https://hzacode.github.io/Athlytics/reference/calculate_acwr_ewma.md)
  : Calculate ACWR using EWMA Method with Confidence Bands
- [`calculate_ef()`](https://hzacode.github.io/Athlytics/reference/calculate_ef.md)
  : Calculate Efficiency Factor (EF)
- [`calculate_ef_from_stream()`](https://hzacode.github.io/Athlytics/reference/calculate_ef_from_stream.md)
  : Calculate EF from Stream Data with Steady-State Analysis
- [`calculate_decoupling()`](https://hzacode.github.io/Athlytics/reference/calculate_decoupling.md)
  : Calculate Aerobic Decoupling
- [`calculate_exposure()`](https://hzacode.github.io/Athlytics/reference/calculate_exposure.md)
  : Calculate Training Load Exposure (ATL, CTL, ACWR)
- [`calculate_pbs()`](https://hzacode.github.io/Athlytics/reference/calculate_pbs.md)
  : Calculate Personal Bests (PBs) from Local Strava Data

## Visualization

Plot and visualize training metrics

- [`plot_acwr()`](https://hzacode.github.io/Athlytics/reference/plot_acwr.md)
  : Plot ACWR Trend
- [`plot_acwr_enhanced()`](https://hzacode.github.io/Athlytics/reference/plot_acwr_enhanced.md)
  : Enhanced ACWR Plot with Confidence Bands and Reference
- [`plot_acwr_comparison()`](https://hzacode.github.io/Athlytics/reference/plot_acwr_comparison.md)
  : Compare RA and EWMA Methods Side-by-Side
- [`plot_ef()`](https://hzacode.github.io/Athlytics/reference/plot_ef.md)
  : Plot Efficiency Factor (EF) Trend
- [`plot_decoupling()`](https://hzacode.github.io/Athlytics/reference/plot_decoupling.md)
  : Plot Aerobic Decoupling Trend
- [`plot_exposure()`](https://hzacode.github.io/Athlytics/reference/plot_exposure.md)
  : Plot Training Load Exposure (ATL vs CTL)
- [`plot_pbs()`](https://hzacode.github.io/Athlytics/reference/plot_pbs.md)
  : Plot Personal Best (PB) Trends
- [`plot_with_reference()`](https://hzacode.github.io/Athlytics/reference/plot_with_reference.md)
  : Plot Individual Metric with Cohort Reference

## Advanced Analysis

Cohort analysis and quality control

- [`cohort_reference()`](https://hzacode.github.io/Athlytics/reference/cohort_reference.md)
  : Calculate Cohort Reference Percentiles
- [`add_reference_bands()`](https://hzacode.github.io/Athlytics/reference/add_reference_bands.md)
  : Add Cohort Reference Bands to Existing Plot
- [`flag_quality()`](https://hzacode.github.io/Athlytics/reference/flag_quality.md)
  : Flag Data Quality Issues in Activity Streams
- [`quality_summary()`](https://hzacode.github.io/Athlytics/reference/quality_summary.md)
  : Get Quality Summary Statistics

## Themes and Colors

Visualization customization

- [`theme_athlytics()`](https://hzacode.github.io/Athlytics/reference/theme_athlytics.md)
  : Get Athlytics Theme
- [`scale_athlytics()`](https://hzacode.github.io/Athlytics/reference/scale_athlytics.md)
  : Apply Color Palette to ggplot
- [`athlytics_colors_acwr_zones()`](https://hzacode.github.io/Athlytics/reference/athlytics_colors_acwr_zones.md)
  : ACWR Zone Colors
- [`athlytics_colors_ef()`](https://hzacode.github.io/Athlytics/reference/athlytics_colors_ef.md)
  : Efficiency Factor Colors
- [`athlytics_colors_training_load()`](https://hzacode.github.io/Athlytics/reference/athlytics_colors_training_load.md)
  : Training Load Colors
- [`athlytics_palette_academic()`](https://hzacode.github.io/Athlytics/reference/athlytics_palette_academic.md)
  : Academic Muted Color Palette
- [`athlytics_palette_cell()`](https://hzacode.github.io/Athlytics/reference/athlytics_palette_cell.md)
  : Cell Journal Palette
- [`athlytics_palette_nature()`](https://hzacode.github.io/Athlytics/reference/athlytics_palette_nature.md)
  : Nature-Inspired Color Palette
- [`athlytics_palette_science()`](https://hzacode.github.io/Athlytics/reference/athlytics_palette_science.md)
  : Science Magazine Palette
- [`athlytics_palette_vibrant()`](https://hzacode.github.io/Athlytics/reference/athlytics_palette_vibrant.md)
  : Vibrant High-Contrast Palette

## Sample Data

Example datasets for testing and learning

- [`athlytics_sample_acwr`](https://hzacode.github.io/Athlytics/reference/athlytics_sample_acwr.md)
  : Sample ACWR Data for Athlytics
- [`athlytics_sample_decoupling`](https://hzacode.github.io/Athlytics/reference/athlytics_sample_decoupling.md)
  : Sample Aerobic Decoupling Data for Athlytics
- [`athlytics_sample_ef`](https://hzacode.github.io/Athlytics/reference/athlytics_sample_ef.md)
  : Sample Efficiency Factor (EF) Data for Athlytics
- [`athlytics_sample_exposure`](https://hzacode.github.io/Athlytics/reference/athlytics_sample_exposure.md)
  : Sample Training Load Exposure Data for Athlytics
- [`athlytics_sample_pbs`](https://hzacode.github.io/Athlytics/reference/athlytics_sample_pbs.md)
  : Sample Personal Bests (PBs) Data for Athlytics
