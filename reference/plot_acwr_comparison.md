# Compare RA and EWMA Methods Side-by-Side

Creates a faceted plot comparing Rolling Average and EWMA ACWR
calculations.

## Usage

``` r
plot_acwr_comparison(
  acwr_ra,
  acwr_ewma,
  title = "ACWR Method Comparison: RA vs EWMA"
)
```

## Arguments

- acwr_ra:

  A data frame from `calculate_acwr_ewma(..., method = "ra")`.

- acwr_ewma:

  A data frame from `calculate_acwr_ewma(..., method = "ewma")`.

- title:

  Plot title. Default "ACWR Method Comparison: RA vs EWMA".

## Value

A ggplot object with faceted comparison.

## Examples

``` r
# Example using sample data
data("sample_acwr", package = "Athlytics")
if (!is.null(sample_acwr) && nrow(sample_acwr) > 0) {
  # Create two versions for comparison (simulate RA vs EWMA)
  acwr_ra <- sample_acwr
  acwr_ewma <- sample_acwr
  acwr_ewma$acwr_smooth <- acwr_ewma$acwr_smooth * runif(nrow(acwr_ewma), 0.95, 1.05)

  p <- plot_acwr_comparison(acwr_ra, acwr_ewma)
  print(p)
}


if (FALSE) { # \dontrun{
activities <- load_local_activities("export.zip")

acwr_ra <- calculate_acwr_ewma(activities, method = "ra")
acwr_ewma <- calculate_acwr_ewma(activities, method = "ewma")

plot_acwr_comparison(acwr_ra, acwr_ewma)
} # }
```
