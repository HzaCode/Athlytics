# Calculate EF from Stream Data with Steady-State Analysis

Calculate efficiency factor (EF) from detailed stream data using
steady-state analysis. This function analyzes heart rate and power/pace
data to find periods of steady effort and calculates the efficiency
factor for those periods.

## Usage

``` r
calculate_ef_from_stream(
  stream_data,
  activity_date,
  act_type,
  ef_metric,
  min_steady_minutes,
  steady_cv_threshold,
  min_hr_coverage,
  quality_control
)
```

## Arguments

- stream_data:

  Data frame with stream data (time, heartrate, watts/distance columns)

- activity_date:

  Date of the activity

- act_type:

  Activity type (e.g., "Run", "Ride")

- ef_metric:

  Efficiency metric to calculate ("pace_hr" or "power_hr")

- min_steady_minutes:

  Minimum duration for steady-state analysis (minutes)

- steady_cv_threshold:

  Coefficient of variation threshold for steady state

- min_hr_coverage:

  Minimum heart rate data coverage required

- quality_control:

  Quality control setting ("off", "flag", "filter")

## Value

Data frame with EF calculation results

## Examples

``` r
if (FALSE) { # \dontrun{
# Parse activity file and calculate EF from streams
streams <- parse_activity_file("activity_12345.fit")
ef_result <- calculate_ef_from_stream(
  stream_data = streams,
  activity_date = as.Date("2025-01-15"),
  act_type = "Run",
  ef_metric = "pace_hr",
  min_steady_minutes = 20,
  steady_cv_threshold = 0.08,
  min_hr_coverage = 0.9,
  quality_control = "filter"
)
} # }
```
