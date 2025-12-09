# Apply Color Palette to ggplot

Helper function to apply Athlytics color palettes to existing plots.

## Usage

``` r
scale_athlytics(palette_name = "nature", type = "color")
```

## Arguments

- palette_name:

  Name of palette: "nature", "academic", "vibrant", "science", or "cell"

- type:

  Either "color" or "fill"

## Value

A ggplot2 scale object (scale_color_manual or scale_fill_manual)

## Examples

``` r
library(ggplot2)
# Apply nature palette to plot
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  scale_athlytics("nature", "color")

```
