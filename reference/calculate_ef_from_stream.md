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

  Efficiency metric to calculate ("speed_hr" or "power_hr")

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
# Example with synthetic stream data
set.seed(42)
n <- 3600
stream <- data.frame(
  time = 0:(n - 1),
  heartrate = round(150 + rnorm(n, 0, 2)),
  velocity_smooth = 3.0 + rnorm(n, 0, 0.05),
  distance = cumsum(rep(3.0, n))
)
result <- calculate_ef_from_stream(
  stream_data = stream,
  activity_date = as.Date("2025-01-15"),
  act_type = "Run",
  ef_metric = "speed_hr",
  min_steady_minutes = 10,
  steady_cv_threshold = 0.1,
  min_hr_coverage = 0.8,
  quality_control = "off"
)
print(result)
#>         date activity_type   ef_value status
#> 1 2025-01-15           Run 0.02000235     ok
```
