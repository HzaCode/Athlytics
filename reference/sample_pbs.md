# Sample Personal Bests (PBs) Data for Athlytics

A dataset containing pre-calculated Personal Best (PB) times for various
distances, derived from simulated Strava data. Used in examples and
tests.

## Usage

``` r
sample_pbs
```

## Format

A tibble with 100 rows and 10 variables:

- activity_id:

  ID of the activity where the effort occurred, as a character string.

- activity_date:

  Date and time of the activity, as a POSIXct object.

- distance:

  Target distance in meters for the best effort, as a numeric value.

- elapsed_time:

  Elapsed time for the effort in seconds, as a numeric value.

- moving_time:

  Moving time for the effort in seconds, as a numeric value.

- time_seconds:

  Typically the same as elapsed_time for best efforts, in seconds, as a
  numeric value.

- cumulative_pb_seconds:

  The personal best time for that distance up to that date, in seconds,
  as a numeric value.

- is_pb:

  Logical, TRUE if this effort set a new personal best.

- distance_label:

  Factor representing the distance (e.g., "1k", "5k").

- time_period:

  Formatted time of the effort, as a Period object from lubridate.

## Source

Simulated data generated for package examples.
