# Calculate Efficiency Factor (EF)

Efficiency Factor measures how much work you perform per unit of
cardiovascular effort. Higher EF indicates better aerobic fitness -
you're able to maintain faster pace or higher power at the same heart
rate. Tracking EF over time helps monitor aerobic base development and
training effectiveness.

**EF Metrics:**

- **pace_hr** (for running): Speed (m/s) / Average HR - Higher values =
  faster pace at same HR = better fitness

- **power_hr** (for cycling): Average Power (watts) / Average HR -
  Higher values = more power at same HR = better fitness

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
  ef_metric = c("pace_hr", "power_hr"),
  start_date = NULL,
  end_date = NULL,
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

  - `"pace_hr"`: Pace-based efficiency (for running). Formula: speed
    (m/s) / avg_HR. Units: m/s/bpm (higher = better fitness)

  - `"power_hr"`: Power-based efficiency (for cycling). Formula:
    avg_watts / avg_HR. Units: W/bpm (higher = better fitness)

  Default: `c("pace_hr", "power_hr")` (uses first matching metric for
  activity type).

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
  m/s/bpm for pace_hr, W/bpm for power_hr.

- status:

  Character. "ok" for successful calculation with stream data,
  "no_streams" for activity-level calculation without stream data,
  "non_steady" if steady-state criteria not met, "insufficient_data" if
  data quality issues, "too_short" if below min_steady_minutes,
  "insufficient_hr_data" if HR coverage below threshold.

## Details

Computes Efficiency Factor (EF) for endurance activities, quantifying
the relationship between performance output (pace or power) and heart
rate. EF is a key indicator of aerobic fitness and training adaptation
(Allen et al., 2019).

**Algorithm:**

1.  Filter activities by type, date range, and minimum duration

2.  For each activity, calculate:

    - pace_hr: (distance / moving_time) / average_heartrate

    - power_hr: average_watts / average_heartrate

3.  Return one EF value per activity

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

**Typical EF Ranges (pace_hr for running):**

- Beginner: 0.01 - 0.015 (m/s per bpm)

- Intermediate: 0.015 - 0.020

- Advanced: 0.020 - 0.025

- Elite: \> 0.025

Note: EF values are relative to individual baseline. Focus on personal
trends rather than absolute comparisons with other athletes.

## References

Allen, H., Coggan, A. R., & McGregor, S. (2019). *Training and Racing
with a Power Meter* (3rd ed.). VeloPress.

## See also

[`plot_ef`](https://hzacode.github.io/Athlytics/reference/plot_ef.md)
for visualization with trend lines,
[`calculate_decoupling`](https://hzacode.github.io/Athlytics/reference/calculate_decoupling.md)
for within-activity efficiency analysis,
[`load_local_activities`](https://hzacode.github.io/Athlytics/reference/load_local_activities.md)
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

if (FALSE) { # \dontrun{
# Example using local Strava export data
activities <- load_local_activities("strava_export_data/activities.csv")

# Calculate Pace/HR efficiency factor for Runs
ef_data_run <- calculate_ef(
  activities_data = activities,
  activity_type = "Run",
  ef_metric = "pace_hr"
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
