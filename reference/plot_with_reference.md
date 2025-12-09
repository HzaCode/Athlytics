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
  bands = c("p25_p75", "p05_p95", "p50")
)
```

## Arguments

- individual:

  A data frame with individual athlete data (from calculate_acwr, etc.)

- reference:

  A data frame from
  [`cohort_reference()`](https://hezhiang.com/Athlytics/reference/cohort_reference.md).

- metric:

  Name of the metric to plot. Default "acwr_smooth".

- date_col:

  Name of the date column. Default "date".

- title:

  Plot title. Default NULL (auto-generated).

- bands:

  Which reference bands to show. Default c("p25_p75", "p05_p95", "p50").

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_with_reference(
  individual = athlete_acwr,
  reference = cohort_ref,
  metric = "acwr_smooth"
)
} # }
```
