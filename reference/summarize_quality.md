# Get Quality Summary Statistics

Provides a summary of quality flags and steady-state segments.

## Usage

``` r
summarize_quality(flagged_streams)

quality_summary(flagged_streams)
```

## Arguments

- flagged_streams:

  A data frame returned by
  [`flag_quality()`](https://hzacode.github.io/Athlytics/reference/flag_quality.md).

## Value

A list with summary statistics:

- total_points:

  Total number of data points

- flagged_points:

  Number of flagged points

- flagged_pct:

  Percentage of flagged points

- steady_state_points:

  Number of steady-state points

- steady_state_pct:

  Percentage in steady-state

- quality_score:

  Overall quality score (0-1)

- hr_spike_pct:

  Percentage with HR spikes

- pw_spike_pct:

  Percentage with power spikes

- gps_drift_pct:

  Percentage with GPS drift

## Examples

``` r
if (FALSE) { # \dontrun{
flagged_data <- flag_quality(stream_data)
summarize_quality(flagged_data)
} # }
```
