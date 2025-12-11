# Get Athlytics Theme

Publication-ready ggplot2 theme with sensible defaults for scientific
figures.

## Usage

``` r
theme_athlytics(base_size = 13, base_family = "")
```

## Arguments

- base_size:

  Base font size (default: 12)

- base_family:

  Font family (default: "")

## Value

A ggplot2 theme object that can be added to plots

## Examples

``` r
# Apply theme to a plot
ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
  ggplot2::geom_point() +
  theme_athlytics()

```
