# Training Load Colors

Colors for acute and chronic training load visualization.

## Usage

``` r
athlytics_colors_training_load()
```

## Value

A named list with three color codes:

- acute:

  Red for short-term load (7-day)

- chronic:

  Blue for long-term load (28-day)

- ratio:

  Teal for ACWR ratio

## Examples

``` r
# Get training load colors
colors <- athlytics_colors_training_load()
colors$acute # Red for acute load
#> [1] "#E64B35"
```
