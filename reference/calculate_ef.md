# Calculate Efficiency Factor (EF)

Efficiency Factor measures how much work you perform per unit of
cardiovascular effort. Higher EF indicates better aerobic fitness -
you're able to maintain faster pace or higher power at the same heart
rate. Tracking EF over time helps monitor aerobic base development and
training effectiveness.

**EF Metrics:**

- **speed_hr** (for running): Speed (m/s) / Average HR

  - Higher values = faster speed at same HR = better fitness

- **power_hr** (for cycling): Average Power (watts) / Average HR

  - Higher values = more power at same HR = better fitness

**What Improves EF?**

- Aerobic base building (Zone 2 training)

- Improved running/cycling economy

- Enhanced cardiovascular efficiency

- Increased mitochondrial density

## Usage

``` r
calculate_ef(
  activities_data,
  activity_type = c("Run", "Ride"),
  ef_metric = c("speed_hr", "gap_hr", "power_hr"),
  start_date = NULL,
  end_date = Sys.Date(),
  min_duration_mins = 20,
  min_steady_minutes = 20,
  steady_cv_threshold = 0.08,
  min_hr_coverage = 0.9,
  quality_control = c("off", "flag", "filter"),
  export_dir = NULL
)
```

## Arguments

- activities_data:

  A data frame of activities from
  [`load_local_activities()`](https://hzacode.github.io/Athlytics/reference/load_local_activities.md).
  Must contain columns: `date`, `type`, `moving_time`, `distance`,
  `average_heartrate`, and `average_watts` (for power_hr metric).

- activity_type:

  Character vector or single string specifying activity type(s) to
  analyze. Common values: `"Run"`, `"Ride"`, or `c("Run", "Ride")`.
  Default: `c("Run", "Ride")`.

- ef_metric:

  Character string specifying the efficiency metric:

  - `"speed_hr"`: Speed-based efficiency (for running). Formula: speed
    (m/s) / avg_HR. Units: m/s/bpm (higher = better fitness)

  - `"gap_hr"`: Grade Adjusted Speed efficiency (for running on hilly
    terrain). Formula: GAP speed (m/s) / avg_HR. Accounts for elevation
    changes. Units: m/s/bpm

  - `"power_hr"`: Power-based efficiency (for cycling). Formula:
    avg_watts / avg_HR. Units: W/bpm (higher = better fitness) Default:
    `c("speed_hr", "power_hr")` (uses first matching metric for activity
    type). Note: `"pace_hr"` is accepted as a deprecated alias for
    `"speed_hr"` for backward compatibility.

- start_date:

  Optional. Analysis start date (YYYY-MM-DD string, Date, or POSIXct).
  Defaults to one year before `end_date`.

- end_date:

  Optional. Analysis end date (YYYY-MM-DD string, Date, or POSIXct).
  Defaults to current date (Sys.Date()).

- min_duration_mins:

  Numeric. Minimum activity duration in minutes to include in analysis
  (default: 20). Filters out very short activities that may not
  represent steady-state aerobic efforts.

- min_steady_minutes:

  Numeric. Minimum duration (minutes) for steady-state segment (default:
  20). Activities shorter than this are automatically rejected for EF
  calculation.

- steady_cv_threshold:

  Numeric. Coefficient of variation threshold for steady-state (default:
  0.08 = 8%). Activities with higher variability are rejected as
  non-steady-state.

- min_hr_coverage:

  Numeric. Minimum HR data coverage threshold (default: 0.9 = 90%).
  Activities with lower HR coverage are rejected as insufficient data
  quality.

- quality_control:

  Character. Quality control mode: "off" (no filtering), "flag" (mark
  issues), or "filter" (exclude flagged data). Default "filter" for
  scientific rigor.

- export_dir:

  Optional. Path to Strava export directory containing activity files.
  When provided, enables stream data analysis for more accurate
  steady-state detection.

## Value

A tibble with the following columns:

- date:

  Activity date (Date class)

- activity_type:

  Activity type (character: "Run" or "Ride")

- ef_value:

  Efficiency Factor value (numeric). Higher = better fitness. Units:
  m/s/bpm for speed_hr, W/bpm for power_hr.

- status:

  Character. "ok" for successful calculation with stream data,
  "no_streams" for activity-level calculation without stream data,
  "non_steady" if steady-state criteria not met, "insufficient_data" if
  data quality issues, "too_short" if below min_steady_minutes,
  "insufficient_hr_data" if HR coverage below threshold.

## Details

Computes Efficiency Factor (EF) for endurance activities, quantifying
the relationship between performance output (speed or power) and heart
rate. EF is a key indicator of aerobic fitness and training adaptation
(Allen et al., 2019).

**Algorithm:**

1.  Filter activities by type, date range, and minimum duration

2.  For each activity, calculate:

    - speed_hr: (distance / moving_time) / average_heartrate

    - power_hr: average_watts / average_heartrate

3.  Return one EF value per activity

**Steady-State Detection Method:**

When stream data is available (via `export_dir`), the function applies a
rolling coefficient of variation (CV) approach to identify steady-state
periods:

1.  **Rolling window**: A sliding window (default 300 seconds, or 1/4 of
    data, min 60 s) computes the rolling mean and standard deviation of
    the output metric (velocity or power).

2.  **CV calculation**: CV = rolling SD / rolling mean at each time
    point.

3.  **Threshold filtering**: Time points where CV \<
    `steady_cv_threshold` (default 8%) are classified as steady-state.

4.  **Minimum duration**: At least `min_steady_minutes` of steady-state
    data must be present; otherwise the activity is marked as
    `"non_steady"`.

5.  **EF computation**: The median EF across all steady-state data
    points is reported.

This approach follows the principle that valid EF comparisons require
quasi-constant output intensity, as outlined by Coyle & González-Alonso
(2001), who demonstrated that cardiovascular drift is meaningful only
under steady-state exercise conditions. The rolling CV method is a
common signal-processing technique for detecting stationarity in
physiological time series.

**Data Quality Considerations:**

- Requires heart rate data (activities without HR are excluded)

- power_hr requires power meter data (cycling with power)

- Best for steady-state endurance efforts (tempo runs, long rides)

- Interval workouts may give misleading EF values

- Environmental factors (heat, altitude) can affect EF

**Interpretation:**

- **Upward trend**: Improving aerobic fitness

- **Stable**: Maintenance phase

- **Downward trend**: Possible overtraining, fatigue, or environmental
  stress

- **Sudden drop**: Check for illness, equipment change, or data quality

**Typical EF Ranges (speed_hr for running):**

- Beginner: 0.01 - 0.015 (m/s per bpm)

- Intermediate: 0.015 - 0.020

- Advanced: 0.020 - 0.025

- Elite: \> 0.025

Note: EF values are relative to individual baseline. Focus on personal
trends rather than absolute comparisons with other athletes.

## References

Allen, H., Coggan, A. R., & McGregor, S. (2019). *Training and Racing
with a Power Meter* (3rd ed.). VeloPress.

Coyle, E. F., & González-Alonso, J. (2001). Cardiovascular drift during
prolonged exercise: New perspectives. *Exercise and Sport Sciences
Reviews*, 29(2), 88-92.
[doi:10.1097/00003677-200104000-00009](https://doi.org/10.1097/00003677-200104000-00009)

## See also

[`plot_ef()`](https://hzacode.github.io/Athlytics/reference/plot_ef.md)
for visualization with trend lines,
[`calculate_decoupling()`](https://hzacode.github.io/Athlytics/reference/calculate_decoupling.md)
for within-activity efficiency analysis,
[`load_local_activities()`](https://hzacode.github.io/Athlytics/reference/load_local_activities.md)
for data loading

## Examples

``` r
# Example using simulated data
data(sample_ef)
print(head(sample_ef))
#> # A tibble: 6 × 3
#>   date       activity_type ef_value
#>   <date>     <chr>            <dbl>
#> 1 2023-01-01 Run               1.16
#> 2 2023-01-01 Ride              1.86
#> 3 2023-01-04 Run               1.19
#> 4 2023-01-06 Run               1.21
#> 5 2023-01-06 Ride              1.94
#> 6 2023-01-08 Run               1.31

# Runnable example with dummy data:
end <- Sys.Date()
dates <- seq(end - 29, end, by = "day")
dummy_activities <- data.frame(
  date = dates,
  type = "Run",
  moving_time = rep(3600, length(dates)), # 1 hour
  distance = rep(10000, length(dates)), # 10 km
  average_heartrate = rep(140, length(dates)),
  average_watts = rep(200, length(dates)),
  weighted_average_watts = rep(210, length(dates)),
  filename = "",
  stringsAsFactors = FALSE
)

# Calculate EF (Speed/HR)
ef_result <- calculate_ef(
  activities_data = dummy_activities,
  activity_type = "Run",
  ef_metric = "speed_hr",
  end_date = end
)
print(head(ef_result))
#>         date activity_type   ef_value     status
#> 1 2026-01-22           Run 0.01984127 no_streams
#> 2 2026-01-23           Run 0.01984127 no_streams
#> 3 2026-01-24           Run 0.01984127 no_streams
#> 4 2026-01-25           Run 0.01984127 no_streams
#> 5 2026-01-26           Run 0.01984127 no_streams
#> 6 2026-01-27           Run 0.01984127 no_streams

if (FALSE) { # \dontrun{
# Example using local Strava export data
activities <- load_local_activities("strava_export_data/activities.csv")

# Calculate Speed/HR efficiency factor for Runs
ef_data_run <- calculate_ef(
  activities_data = activities,
  activity_type = "Run",
  ef_metric = "speed_hr"
)
print(tail(ef_data_run))

# Calculate Power/HR efficiency factor for Rides
ef_data_ride <- calculate_ef(
  activities_data = activities,
  activity_type = "Ride",
  ef_metric = "power_hr"
)
print(tail(ef_data_ride))
} # }
```
