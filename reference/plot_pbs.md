# Plot Personal Best (PB) Trends

Visualizes the trend of personal best times for specific running
distances.

## Usage

``` r
plot_pbs(
  data,
  activity_type = "Run",
  distance_meters,
  max_activities = 500,
  date_range = NULL,
  add_trend_line = TRUE,
  pbs_df = NULL
)
```

## Arguments

- data:

  **Recommended: Pass pre-calculated data via `pbs_df` (local export
  preferred).** A data frame from
  [`calculate_pbs()`](https://hzacode.github.io/Athlytics/reference/calculate_pbs.md)
  or activities data from
  [`load_local_activities()`](https://hzacode.github.io/Athlytics/reference/load_local_activities.md).

- activity_type:

  Type(s) of activities to search (e.g., "Run"). Default "Run".

- distance_meters:

  Numeric vector of distances (meters) to plot PBs for (e.g.,
  `c(1000, 5000)`). Relies on Strava's `best_efforts` data.

- max_activities:

  Max number of recent activities to check. Default 500. Reduce for
  speed.

- date_range:

  Optional. Filter activities by date `c("YYYY-MM-DD", "YYYY-MM-DD")`.

- add_trend_line:

  Logical. Whether to add a trend line to the plot. Default TRUE.

- pbs_df:

  **Recommended.** A pre-calculated data frame from
  [`calculate_pbs()`](https://hzacode.github.io/Athlytics/reference/calculate_pbs.md).
  When provided, analysis uses local data only (no API calls).

## Value

A ggplot object showing PB trends, faceted by distance if multiple are
plotted.

## Details

Plots the trend of best efforts for specified distances, highlighting
new PBs. **Recommended workflow: Use local data via `pbs_df`.**

Visualizes data from `calculate_pbs`. Points show best efforts; solid
points mark new PBs. Y-axis is MM:SS. **Best practice: Use
[`load_local_activities()`](https://hzacode.github.io/Athlytics/reference/load_local_activities.md) +
[`calculate_pbs()`](https://hzacode.github.io/Athlytics/reference/calculate_pbs.md) +
this function.** Legacy API mode is maintained for backward
compatibility only.

## Examples

``` r
# Example using the built-in sample data
# This data now contains a simulated history of performance improvements
data("athlytics_sample_pbs", package = "Athlytics")

if (!is.null(athlytics_sample_pbs) && nrow(athlytics_sample_pbs) > 0) {
  # Plot PBs using the package sample data directly
  p <- plot_pbs(pbs_df = athlytics_sample_pbs, activity_type = "Run")
  print(p)
}
#> Generating plot...
#> `geom_smooth()` using formula = 'y ~ x'


if (FALSE) {
# Example using local Strava export data
activities <- load_local_activities("strava_export_data/activities.csv")

# Plot PBS trend for Runs (last 6 months)
pb_data_run <- calculate_pbs(activities_data = activities, 
                             activity_type = "Run", 
                             distance_meters = c(1000,5000,10000), 
                             date_range = c(format(Sys.Date() - months(6)),
                                          format(Sys.Date())))
if(nrow(pb_data_run) > 0) {
  plot_pbs(pbs_df = pb_data_run, distance_meters = c(1000,5000,10000))
}

# Plot PBS trend for Rides (if applicable, though PBs are mainly for Runs)
pb_data_ride <- calculate_pbs(activities_data = activities, 
                               activity_type = "Ride", 
                               distance_meters = c(10000, 20000))
if(nrow(pb_data_ride) > 0) {
   plot_pbs(pbs_df = pb_data_ride, distance_meters = c(10000, 20000))
}

# Plot PBS trend for multiple Run types (no trend line)
pb_data_multi <- calculate_pbs(activities_data = activities, 
                               activity_type = c("Run", "VirtualRun"), 
                               distance_meters = c(1000,5000))
if(nrow(pb_data_multi) > 0) {
  plot_pbs(pbs_df = pb_data_multi, distance_meters = c(1000,5000), 
           add_trend_line = FALSE)
}
}
```
