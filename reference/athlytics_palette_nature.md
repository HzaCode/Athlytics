# Nature-Inspired Color Palette

Professional, colorblind-friendly palette based on Nature journal's
visualization guidelines. Suitable for multi-series plots.

## Usage

``` r
athlytics_palette_nature()
```

## Value

A character vector of 9 hex color codes

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot2::ggplot(data, ggplot2::aes(x, y, color = group)) +
  ggplot2::geom_line() +
  ggplot2::scale_color_manual(values = athlytics_palette_nature())
} # }
```
