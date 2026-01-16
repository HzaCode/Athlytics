# Plot Aerobic Decoupling Trend

Visualizes the trend of aerobic decoupling over time.

## Usage

``` r
plot_decoupling(
  data,
  activity_type = c("Run", "Ride"),
  decouple_metric = c("pace_hr", "power_hr"),
  start_date = NULL,
  end_date = NULL,
  min_duration_mins = 45,
  add_trend_line = TRUE,
  smoothing_method = "loess",
  decoupling_df = NULL
)
```

## Arguments

- data:

  **Recommended: Pass pre-calculated data via `decoupling_df` (local
  export preferred).** A data frame from
  [`calculate_decoupling()`](https://hzacode.github.io/Athlytics/reference/calculate_decoupling.md)
  or activities data from
  [`load_local_activities()`](https://hzacode.github.io/Athlytics/reference/load_local_activities.md).

- activity_type:

  Type(s) of activities to analyze (e.g., "Run", "Ride").

- decouple_metric:

  Metric basis: "pace_hr" or "power_hr".

- start_date:

  Optional. Analysis start date (YYYY-MM-DD string or Date). Defaults to
  ~1 year ago.

- end_date:

  Optional. Analysis end date (YYYY-MM-DD string or Date). Defaults to
  today.

- min_duration_mins:

  Minimum activity duration (minutes) to include. Default 45.

- add_trend_line:

  Add a smoothed trend line (`geom_smooth`)? Default `TRUE`.

- smoothing_method:

  Smoothing method for trend line (e.g., "loess", "lm"). Default
  "loess".

- decoupling_df:

  **Recommended.** A pre-calculated data frame from
  [`calculate_decoupling()`](https://hzacode.github.io/Athlytics/reference/calculate_decoupling.md).
  When provided, analysis uses local data only (no API calls). Must
  contain 'date' and 'decoupling' columns.

## Value

A ggplot object showing the decoupling trend.

## Details

Plots the aerobic decoupling trend over time. **Recommended workflow:
Use local data via `decoupling_df`.**

Plots decoupling percentage ((EF_1st_half - EF_2nd_half) / EF_1st_half
\* 100). Positive values mean HR drifted relative to output. A 5\\
threshold line is often used as reference. **Best practice: Use
[`load_local_activities()`](https://hzacode.github.io/Athlytics/reference/load_local_activities.md) +
[`calculate_decoupling()`](https://hzacode.github.io/Athlytics/reference/calculate_decoupling.md) +
this function.**

## Examples

``` r
# Example using pre-calculated sample data
data("sample_decoupling", package = "Athlytics")
p <- plot_decoupling(decoupling_df = sample_decoupling)
#> Generating plot...
print(p)
#> `geom_smooth()` using formula = 'y ~ x'


if (FALSE) { # \dontrun{
# Example using local Strava export data
activities <- load_local_activities("strava_export_data/activities.csv")

# Example 1: Plot Decoupling trend for Runs (last 6 months)
decoupling_runs_6mo <- calculate_decoupling(
  activities_data = activities,
  export_dir = "strava_export_data",
  activity_type = "Run",
  decouple_metric = "pace_hr",
  start_date = Sys.Date() - months(6)
)
plot_decoupling(decoupling_runs_6mo)

# Example 2: Plot Decoupling trend for Rides
decoupling_rides <- calculate_decoupling(
  activities_data = activities,
  export_dir = "strava_export_data",
  activity_type = "Ride",
  decouple_metric = "power_hr"
)
plot_decoupling(decoupling_rides)

# Example 3: Plot Decoupling trend for multiple Run types (no trend line)
decoupling_multi_run <- calculate_decoupling(
  activities_data = activities,
  export_dir = "strava_export_data",
  activity_type = c("Run", "VirtualRun"),
  decouple_metric = "pace_hr"
)
plot_decoupling(decoupling_multi_run, add_trend_line = FALSE)
} # }
```
