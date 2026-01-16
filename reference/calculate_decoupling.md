# Calculate Aerobic Decoupling

Calculates aerobic decoupling for Strava activities from local export
data.

## Usage

``` r
calculate_decoupling(
  activities_data = NULL,
  export_dir = "strava_export_data",
  activity_type = c("Run", "Ride"),
  decouple_metric = c("pace_hr", "power_hr"),
  start_date = NULL,
  end_date = NULL,
  min_duration_mins = 40,
  min_steady_minutes = 40,
  steady_cv_threshold = 0.08,
  min_hr_coverage = 0.9,
  quality_control = c("off", "flag", "filter"),
  stream_df = NULL
)
```

## Arguments

- activities_data:

  A data frame from
  [`load_local_activities()`](https://hzacode.github.io/Athlytics/reference/load_local_activities.md).
  Required unless `stream_df` is provided.

- export_dir:

  Base directory of Strava export containing the activities folder.
  Default is "strava_export_data".

- activity_type:

  Type(s) of activities to analyze (e.g., "Run", "Ride").

- decouple_metric:

  Basis for calculation: "pace_hr" or "power_hr" (legacy
  "pace_hr"/"power_hr" also supported).

- start_date:

  Optional. Analysis start date (YYYY-MM-DD string or Date). Defaults to
  one year ago.

- end_date:

  Optional. Analysis end date (YYYY-MM-DD string or Date). Defaults to
  today.

- min_duration_mins:

  Minimum activity duration (minutes) to include. Default 40.

- min_steady_minutes:

  Minimum duration (minutes) for steady-state segment (default: 40).
  Activities shorter than this are automatically rejected for decoupling
  calculation.

- steady_cv_threshold:

  Coefficient of variation threshold for steady-state (default: 0.08 =
  8%). Activities with higher variability are rejected as
  non-steady-state.

- min_hr_coverage:

  Minimum HR data coverage threshold (default: 0.9 = 90%). Activities
  with lower HR coverage are rejected as insufficient data quality.

- quality_control:

  Quality control mode: "off" (no filtering), "flag" (mark issues), or
  "filter" (exclude flagged data). Default "filter" for scientific
  rigor.

- stream_df:

  Optional. A pre-fetched data frame for a *single* activity's stream.
  If provided, calculates decoupling for this data directly, ignoring
  other parameters. Must include columns: `time`, `heartrate`, and
  either `velocity_smooth`/`distance` (for pace_hr) or `watts` (for
  power_hr).

## Value

Returns a data frame with columns:

- date:

  Activity date (Date class)

- decoupling:

  Decoupling percentage (\\). Positive = HR drift, negative = improved
  efficiency

- status:

  Character. "ok" for successful calculation, "non_steady" if
  steady-state criteria not met, "insufficient_data" if data quality
  issues

OR a single numeric decoupling value if `stream_df` is provided.

## Details

Calculates aerobic decoupling (HR drift relative to pace/power) using
detailed activity stream data from local FIT/TCX/GPX files.

Provides data for `plot_decoupling`. Compares output/HR efficiency
between first and second halves of activities. Positive values indicate
HR drift (cardiovascular drift).

**Best practice**: Use
[`load_local_activities()`](https://hzacode.github.io/Athlytics/reference/load_local_activities.md)
to load data, then pass to this function.

The function parses FIT/TCX/GPX files from your Strava export to extract
detailed stream data (time, heartrate, distance/power). Activities are
split into two halves, and the efficiency factor (output/HR) is compared
between halves.

## Examples

``` r
# Example using simulated data
data(sample_decoupling)
print(head(sample_decoupling))
#> # A tibble: 6 × 2
#>   date       decoupling
#>   <date>          <dbl>
#> 1 2023-01-01       13.4
#> 2 2023-01-06       13.6
#> 3 2023-01-14       12.9
#> 4 2023-01-22       13.0
#> 5 2023-01-27       10.6
#> 6 2023-02-02       14.3

if (FALSE) { # \dontrun{
# Load local activities
activities <- load_local_activities("strava_export_data/activities.csv")

# Calculate Pace/HR decoupling for recent runs
run_decoupling <- calculate_decoupling(
  activities_data = activities,
  export_dir = "strava_export_data",
  activity_type = "Run",
  decouple_metric = "pace_hr",
  start_date = "2024-01-01"
)
print(tail(run_decoupling))

# Calculate for a single activity stream
# stream_data <- parse_activity_file("strava_export_data/activities/12345.fit")
# single_decoupling <- calculate_decoupling(stream_df = stream_data, decouple_metric = "pace_hr")
} # }
```
