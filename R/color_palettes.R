# R/color_palettes.R
# Internal color palettes for Athlytics
# Default colors inspired by Nature journal guidelines
# All plots use these colors automatically - no user configuration needed

#' @keywords internal
NULL

#' Nature-Inspired Color Palette
#'
#' Professional, colorblind-friendly palette based on Nature journal's
#' visualization guidelines. Suitable for multi-series plots.
#'
#' @return A character vector of 9 hex color codes
#'
#' @export
#' @examples
#' # View the palette colors
#' athlytics_palette_nature()
#'
#' # Display as color swatches
#' barplot(rep(1, 9), col = athlytics_palette_nature(), border = NA)
athlytics_palette_nature <- function() {
  c(
    "#E64B35", # Red (primary data)
    "#4DBBD5", # Cyan (secondary data)
    "#00A087", # Teal (tertiary)
    "#3C5488", # Navy blue
    "#F39B7F", # Coral
    "#8491B4", # Slate blue
    "#91D1C2", # Mint green
    "#DC0000", # Dark red
    "#7E6148" # Brown
  )
}
#' Vibrant High-Contrast Palette
#'
#' High-saturation palette optimized for presentations and posters.
#' Maximum visual impact while maintaining colorblind accessibility.
#'
#' @return A character vector of 8 hex color codes
#'
#' @export
#' @examples
#' # View the palette colors
#' athlytics_palette_vibrant()
athlytics_palette_vibrant <- function() {
  c(
    "#FF6B6B", # Coral red
    "#4ECDC4", # Turquoise
    "#45B7D1", # Sky blue
    "#FFA07A", # Light salmon
    "#98D8C8", # Seafoam
    "#FFE66D", # Yellow
    "#A8E6CF", # Mint
    "#FF8B94" # Pink
  )
}

#' Get Athlytics Theme
#'
#' Publication-ready ggplot2 theme with sensible defaults for scientific figures.
#'
#' @param base_size Base font size (default: 12)
#' @param base_family Font family (default: "")
#'
#' @return A ggplot2 theme object that can be added to plots
#'
#' @examples
#' # Apply theme to a plot
#' ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
#'   ggplot2::geom_point() +
#'   theme_athlytics()
#'
#' @export
#' @import ggplot2
theme_athlytics <- function(base_size = 13, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Plot background - clean white
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),

      # Plot titles - modern, bold, left-aligned
      plot.title = ggplot2::element_text(
        face = "bold",
        size = base_size * 1.4,
        hjust = 0,
        color = "#2c3e50",
        margin = ggplot2::margin(b = base_size * 0.8)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size * 0.95,
        color = "#7f8c8d",
        hjust = 0,
        lineheight = 1.2,
        margin = ggplot2::margin(b = base_size * 1)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size * 0.75,
        color = "#95a5a6",
        hjust = 1,
        margin = ggplot2::margin(t = base_size * 0.8)
      ),
      plot.margin = ggplot2::margin(t = 15, r = 15, b = 15, l = 15),

      # Axes - clean and minimal
      axis.title.x = ggplot2::element_text(
        size = base_size * 1.05,
        face = "bold",
        color = "#34495e",
        margin = ggplot2::margin(t = base_size * 0.6)
      ),
      axis.title.y = ggplot2::element_text(
        size = base_size * 1.05,
        face = "bold",
        color = "#34495e",
        margin = ggplot2::margin(r = base_size * 0.6)
      ),
      axis.text.x = ggplot2::element_text(
        size = base_size * 0.9,
        color = "#7f8c8d",
        margin = ggplot2::margin(t = base_size * 0.3)
      ),
      axis.text.y = ggplot2::element_text(
        size = base_size * 0.9,
        color = "#7f8c8d",
        margin = ggplot2::margin(r = base_size * 0.3)
      ),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(color = "#bdc3c7", linewidth = 0.3),
      axis.ticks.length = ggplot2::unit(4, "pt"),

      # Legend - modern and spacious
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(
        size = base_size * 0.95,
        face = "bold",
        color = "#34495e"
      ),
      legend.text = ggplot2::element_text(
        size = base_size * 0.85,
        color = "#7f8c8d"
      ),
      legend.key.size = ggplot2::unit(1.2, "lines"),
      legend.key = ggplot2::element_blank(),
      legend.spacing.x = ggplot2::unit(0.5, "lines"),
      legend.box.spacing = ggplot2::unit(0.5, "lines"),
      legend.margin = ggplot2::margin(t = base_size),

      # Panel grid - subtle and elegant
      panel.grid.major = ggplot2::element_line(
        color = "#ecf0f1",
        linewidth = 0.5,
        linetype = "solid"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(1.5, "lines"),

      # Strip (for facets) - clean and modern
      strip.text = ggplot2::element_text(
        size = base_size * 1.05,
        face = "bold",
        color = "#2c3e50",
        margin = ggplot2::margin(b = base_size * 0.5, t = base_size * 0.5)
      ),
      strip.background = ggplot2::element_rect(
        fill = "#f8f9fa",
        color = "#dee2e6",
        linewidth = 0.5
      )
    )
}
