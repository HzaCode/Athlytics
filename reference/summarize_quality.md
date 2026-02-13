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
# Create sample stream and summarize quality
set.seed(42)
stream_data <- data.frame(
  time = seq(0, 3600, by = 1),
  heartrate = pmax(60, pmin(200, rnorm(3601, mean = 150, sd = 10))),
  watts = pmax(0, rnorm(3601, mean = 200, sd = 20)),
  velocity_smooth = pmax(0, rnorm(3601, mean = 3.5, sd = 0.3))
)
flagged_data <- flag_quality(stream_data, sport = "Run")
summarize_quality(flagged_data)
#> $total_points
#> [1] 3601
#> 
#> $flagged_points
#> [1] 1713
#> 
#> $flagged_pct
#> [1] 47.57
#> 
#> $steady_state_points
#> [1] 0
#> 
#> $steady_state_pct
#> [1] 0
#> 
#> $quality_score
#> [1] 0.524
#> 
#> $hr_spike_pct
#> [1] 47.57
#> 
#> $pw_spike_pct
#> [1] 0
#> 
#> $gps_drift_pct
#> [1] 0
#> 
```
