# Athlytics: Academic R Package for Sports Physiology Analysis from Local 'Strava' Data

An open-source computational framework for longitudinal analysis of
exercise physiology metrics using local 'Strava' data exports. Designed
for personal analysis and sports science applications, this package
provides standardized functions to calculate and visualize key
physiological indicators including Acute:Chronic Workload Ratio (ACWR),
Efficiency Factor (EF), and training load metrics.

Athlytics is an open-source computational framework for longitudinal
analysis of exercise physiology metrics using local Strava data exports.
Designed for personal analysis and sports science applications, this
package provides standardized functions to calculate and visualize key
physiological indicators.

## Main Functions

**Data Loading:**

- [`load_local_activities()`](https://hzacode.github.io/Athlytics/reference/load_local_activities.md):
  Load activities from Strava export ZIP or directory

- [`parse_activity_file()`](https://hzacode.github.io/Athlytics/reference/parse_activity_file.md):
  Parse individual FIT/TCX/GPX files

**Training Load Analysis:**

- [`calculate_acwr()`](https://hzacode.github.io/Athlytics/reference/calculate_acwr.md):
  Calculate Acute:Chronic Workload Ratio

- [`calculate_acwr_ewma()`](https://hzacode.github.io/Athlytics/reference/calculate_acwr_ewma.md):
  ACWR using exponentially weighted moving averages

- [`calculate_exposure()`](https://hzacode.github.io/Athlytics/reference/calculate_exposure.md):
  Calculate training load exposure metrics

**Physiological Metrics:**

- [`calculate_ef()`](https://hzacode.github.io/Athlytics/reference/calculate_ef.md):
  Calculate Efficiency Factor (EF)

- [`calculate_decoupling()`](https://hzacode.github.io/Athlytics/reference/calculate_decoupling.md):
  Calculate cardiovascular decoupling

- [`calculate_pbs()`](https://hzacode.github.io/Athlytics/reference/calculate_pbs.md):
  Calculate personal bests

**Visualization:**

- [`plot_acwr()`](https://hzacode.github.io/Athlytics/reference/plot_acwr.md),
  [`plot_acwr_enhanced()`](https://hzacode.github.io/Athlytics/reference/plot_acwr_enhanced.md):
  Plot ACWR trends

- [`plot_ef()`](https://hzacode.github.io/Athlytics/reference/plot_ef.md):
  Plot Efficiency Factor trends

- [`plot_decoupling()`](https://hzacode.github.io/Athlytics/reference/plot_decoupling.md):
  Plot decoupling analysis

- [`plot_exposure()`](https://hzacode.github.io/Athlytics/reference/plot_exposure.md):
  Plot training load exposure

- [`plot_pbs()`](https://hzacode.github.io/Athlytics/reference/plot_pbs.md):
  Plot personal bests progression

**Quality Control & Cohort Analysis:**

- [`flag_quality()`](https://hzacode.github.io/Athlytics/reference/flag_quality.md):
  Flag activities based on quality criteria

- [`summarize_quality()`](https://hzacode.github.io/Athlytics/reference/summarize_quality.md):
  Summarize stream quality flags

- [`calculate_cohort_reference()`](https://hzacode.github.io/Athlytics/reference/calculate_cohort_reference.md):
  Generate cohort reference bands

## Sample Datasets

The package includes simulated datasets for examples and testing:

- [sample_acwr](https://hzacode.github.io/Athlytics/reference/sample_acwr.md):
  Sample ACWR data

- [sample_ef](https://hzacode.github.io/Athlytics/reference/sample_ef.md):
  Sample Efficiency Factor data

- [sample_decoupling](https://hzacode.github.io/Athlytics/reference/sample_decoupling.md):
  Sample decoupling data

- [sample_exposure](https://hzacode.github.io/Athlytics/reference/sample_exposure.md):
  Sample exposure data

- [sample_pbs](https://hzacode.github.io/Athlytics/reference/sample_pbs.md):
  Sample personal bests data

## Getting Started

    library(Athlytics)

    # Load your Strava export
    activities <- load_local_activities("path/to/strava_export.zip")

    # Calculate ACWR
    acwr_data <- calculate_acwr(activities, activity_type = "Run")

    # Visualize
    plot_acwr(acwr_data)

## See also

Useful links:

- <https://hzacode.github.io/Athlytics/>

- <https://github.com/HzaCode/Athlytics>

- Report bugs at <https://github.com/HzaCode/Athlytics/issues>

&nbsp;

- Package website: <https://hzacode.github.io/Athlytics/>

- GitHub repository: <https://github.com/HzaCode/Athlytics>

- Strava: <https://www.strava.com/>

## Author

**Maintainer**: Zhiang He <ang@hezhiang.com>
