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
  [`calculate_pbs()`](https://hezhiang.com/Athlytics/reference/calculate_pbs.md)
  or activities data from
  [`load_local_activities()`](https://hezhiang.com/Athlytics/reference/load_local_activities.md).

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
  [`calculate_pbs()`](https://hezhiang.com/Athlytics/reference/calculate_pbs.md).
  When provided, analysis uses local data only (no API calls).

## Value

A ggplot object showing PB trends, faceted by distance if multiple are
plotted.

## Details

Plots the trend of best efforts for specified distances, highlighting
new PBs. **Recommended workflow: Use local data via `pbs_df`.**

Visualizes data from `calculate_pbs`. Points show best efforts; solid
points mark new PBs. Y-axis is MM:SS. **Best practice: Use
[`load_local_activities()`](https://hezhiang.com/Athlytics/reference/load_local_activities.md) +
[`calculate_pbs()`](https://hezhiang.com/Athlytics/reference/calculate_pbs.md) +
this function.** Legacy API mode is maintained for backward
compatibility only.

## Examples

``` r
# Example using simulated data
data(athlytics_sample_pbs)
# athlytics_sample_pbs should contain the PBs to be plotted
if (!is.null(athlytics_sample_pbs) && nrow(athlytics_sample_pbs) > 0) {
  sample_pbs_for_plot <- athlytics_sample_pbs
  
  # Ensure the date column is named 'activity_date' and is of Date type for plot_pbs
  if ("date" %in% names(sample_pbs_for_plot) && !"activity_date" %in% names(sample_pbs_for_plot)) {
    names(sample_pbs_for_plot)[names(sample_pbs_for_plot) == "date"] <- "activity_date"
  }
  if ("activity_date" %in% names(sample_pbs_for_plot)) {
    sample_pbs_for_plot$activity_date <- as.Date(sample_pbs_for_plot$activity_date)
  } else {
    message("Relevant date column not found in sample PBs for example.")
  }
  
  # plot_pbs requires distance_meters. Extract from sample data.
  req_dist_meters <- NULL
  if ("distance" %in% names(sample_pbs_for_plot)) {
    req_dist_meters <- unique(sample_pbs_for_plot$distance)
  } else if ("distance_target_m" %in% names(sample_pbs_for_plot)) {
    req_dist_meters <- unique(sample_pbs_for_plot$distance_target_m)
  }
  
  can_plot <- "activity_date" %in% names(sample_pbs_for_plot) && 
              !is.null(req_dist_meters) && length(req_dist_meters) > 0

  if (can_plot) {
    p <- plot_pbs(pbs_df = sample_pbs_for_plot, activity_type = "Run", 
                  distance_meters = req_dist_meters)
    print(p)
  } else {
    message("Sample PBs data lacks required date or distance info for example.")
  }
} else {
  message("athlytics_sample_pbs is empty or not found, skipping example plot.")
}
#> Generating plot...
#> `geom_smooth()` using formula = 'y ~ x'


if (FALSE) { # \dontrun{
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
} # }
```
