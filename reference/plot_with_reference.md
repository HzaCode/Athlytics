# Plot Individual Metric with Cohort Reference

Creates a complete plot showing an individual's metric trend with cohort
reference percentile bands.

## Usage

``` r
plot_with_reference(
  individual,
  reference,
  metric = "acwr_smooth",
  date_col = "date",
  title = NULL,
  bands = c("p25_p75", "p05_p95", "p50"),
  caption = NULL
)
```

## Arguments

- individual:

  A data frame with individual athlete data (from calculate_acwr, etc.)

- reference:

  A data frame from
  [`calculate_cohort_reference()`](https://hzacode.github.io/Athlytics/reference/calculate_cohort_reference.md).

- metric:

  Name of the metric to plot. Default "acwr_smooth".

- date_col:

  Name of the date column. Default "date".

- title:

  Plot title. Default NULL (auto-generated).

- bands:

  Which reference bands to show. Default c("p25_p75", "p05_p95", "p50").

- caption:

  Plot caption. Default NULL (no caption).

## Value

A ggplot object.

## Examples

``` r
# Example with weekly data for smooth curves
set.seed(123)
n_weeks <- 40
dates <- seq(as.Date("2023-01-01"), by = "week", length.out = n_weeks)

# Individual athlete data with realistic ACWR fluctuation
individual_data <- data.frame(
  date = dates,
  acwr_smooth = 1.0 + cumsum(rnorm(n_weeks, 0, 0.03))
)

# Cohort reference percentiles with gradual variation
base_trend <- 1.0 + cumsum(rnorm(n_weeks, 0, 0.015))
reference_data <- data.frame(
  date = rep(dates, each = 5),
  percentile = rep(c("p05", "p25", "p50", "p75", "p95"), n_weeks),
  value = as.vector(t(outer(base_trend, c(-0.35, -0.15, 0, 0.15, 0.35), "+")))
)

p <- plot_with_reference(
  individual = individual_data,
  reference = reference_data,
  metric = "acwr_smooth"
)
print(p)


if (FALSE) { # \dontrun{
plot_with_reference(
  individual = athlete_acwr,
  reference = cohort_ref,
  metric = "acwr_smooth"
)
} # }
```
