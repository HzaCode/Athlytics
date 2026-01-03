# Add Cohort Reference Bands to Existing Plot

Adds percentile reference bands from a cohort to an individual's metric
plot.

## Usage

``` r
add_reference_bands(
  p,
  reference_data,
  bands = c("p25_p75", "p05_p95", "p50"),
  alpha = 0.15,
  colors = list(p25_p75 = "#440154FF", p05_p95 = "#3B528BFF", p50 = "#21908CFF")
)
```

## Arguments

- p:

  A ggplot object (typically from plot_acwr or similar).

- reference_data:

  A data frame from
  [`cohort_reference()`](https://hzacode.github.io/Athlytics/reference/cohort_reference.md).

- bands:

  Character vector specifying which bands to plot. Options: "p25_p75"
  (inner quartiles), "p05_p95" (outer 5-95 range), "p50" (median).
  Default c("p25_p75", "p05_p95", "p50").

- alpha:

  Transparency for reference bands (0-1). Default 0.15.

- colors:

  Named list of colors for bands. Default uses viridis colors.

## Value

A ggplot object with added reference bands.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create base plot
p <- plot_acwr(acwr_df = individual_acwr)

# Add reference bands
p_with_ref <- add_reference_bands(p, reference_data = cohort_ref)
print(p_with_ref)
} # }
```
