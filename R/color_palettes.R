# R/color_palettes.R
# Internal color palettes for Athlytics
# Default colors inspired by Nature, Science, and Cell journal guidelines
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
#' \dontrun{
#' ggplot2::ggplot(data, ggplot2::aes(x, y, color = group)) +
#'   ggplot2::geom_line() +
#'   ggplot2::scale_color_manual(values = athlytics_palette_nature())
#' }
athlytics_palette_nature <- function() {
  c(
    "#E64B35",  # Red (primary data)
    "#4DBBD5",  # Cyan (secondary data)
    "#00A087",  # Teal (tertiary)
    "#3C5488",  # Navy blue
    "#F39B7F",  # Coral
    "#8491B4",  # Slate blue
    "#91D1C2",  # Mint green
    "#DC0000",  # Dark red
    "#7E6148"   # Brown
  )
}

#' Academic Muted Color Palette
#'
#' Low-saturation, elegant palette suitable for formal publications and
#' technical reports. Emphasizes clarity over visual impact.
#'
#' @return A character vector of 8 hex color codes
#'
#' @export
#' @examples
#' \dontrun{
#' ggplot2::ggplot(data, ggplot2::aes(x, y, color = group)) +
#'   ggplot2::geom_line() +
#'   ggplot2::scale_color_manual(values = athlytics_palette_academic())
#' }
athlytics_palette_academic <- function() {
  c(
    "#8C7A6B",  # Taupe
    "#A8968E",  # Warm gray
    "#C4B5A8",  # Light beige
    "#7B9FA3",  # Steel blue
    "#9DB4B8",  # Powder blue
    "#B8A68E",  # Sand
    "#8B9E9F",  # Sage
    "#D4C4B0"   # Light stone (unique shade)
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
#' \dontrun{
#' ggplot2::ggplot(data, ggplot2::aes(x, y, fill = category)) +
#'   ggplot2::geom_bar(stat = "identity") +
#'   ggplot2::scale_fill_manual(values = athlytics_palette_vibrant())
#' }
athlytics_palette_vibrant <- function() {
  c(
    "#FF6B6B",  # Coral red
    "#4ECDC4",  # Turquoise
    "#45B7D1",  # Sky blue
    "#FFA07A",  # Light salmon
    "#98D8C8",  # Seafoam
    "#FFE66D",  # Yellow
    "#A8E6CF",  # Mint
    "#FF8B94"   # Pink
  )
}

#' Science Magazine Palette
#'
#' Classic palette inspired by Science magazine's figure guidelines.
#' Conservative and widely accepted in scientific community.
#'
#' @return A character vector of 8 hex color codes
#'
#' @examples
#' # Get Science palette colors
#' colors <- athlytics_palette_science()
#' colors[1]  # Dark blue
#'
#' @export
athlytics_palette_science <- function() {
  c(
    "#003F5C",  # Dark blue
    "#58508D",  # Purple
    "#BC5090",  # Magenta
    "#FF6361",  # Coral
    "#FFA600",  # Orange
    "#7A5195",  # Plum
    "#EF5675",  # Rose
    "#FFC300"   # Gold
  )
}

#' Cell Journal Palette
#'
#' Modern palette based on Cell Press visualization standards.
#' Balances professional appearance with visual clarity.
#'
#' @return A character vector of 8 hex color codes
#'
#' @examples
#' # Get Cell palette colors
#' colors <- athlytics_palette_cell()
#' colors[1]  # Blue
#'
#' @export
athlytics_palette_cell <- function() {
  c(
    "#0173B2",  # Blue
    "#DE8F05",  # Orange
    "#029E73",  # Green
    "#CC78BC",  # Purple
    "#CA9161",  # Tan
    "#949494",  # Gray
    "#ECE133",  # Yellow
    "#56B4E9"   # Light blue
  )
}

#' ACWR Zone Colors
#'
#' Standardized colors for ACWR risk zones following sports science conventions.
#'
#' @return A named list with four color codes for ACWR zones:
#'   \item{undertraining}{Light blue for low load}
#'   \item{safe}{Green for optimal training zone}
#'   \item{caution}{Orange for moderate risk}
#'   \item{high_risk}{Red for high injury risk}
#'
#' @examples
#' # Get ACWR zone colors
#' colors <- athlytics_colors_acwr_zones()
#' colors$safe  # Returns green color code
#'
#' @export
athlytics_colors_acwr_zones <- function() {
  list(
    undertraining = "#AED6F1",  # Light blue
    safe = "#A9DFBF",           # Green
    caution = "#FAD7A0",        # Orange
    high_risk = "#F5B7B1"       # Red
  )
}

#' Training Load Colors
#'
#' Colors for acute and chronic training load visualization.
#'
#' @return A named list with three color codes:
#'   \item{acute}{Red for short-term load (7-day)}
#'   \item{chronic}{Blue for long-term load (28-day)}
#'   \item{ratio}{Teal for ACWR ratio}
#'
#' @examples
#' # Get training load colors
#' colors <- athlytics_colors_training_load()
#' colors$acute  # Red for acute load
#'
#' @export
athlytics_colors_training_load <- function() {
  list(
    acute = "#E64B35",    # Red (short-term)
    chronic = "#4DBBD5",  # Blue (long-term)
    ratio = "#00A087"     # Teal (ACWR)
  )
}

#' Efficiency Factor Colors
#'
#' Colors for efficiency factor trends by activity type.
#'
#' @return A named list with four color codes by sport:
#'   \item{run}{Navy blue for running}
#'   \item{ride}{Coral for cycling}
#'   \item{swim}{Cyan for swimming}
#'   \item{other}{Slate for other activities}
#'
#' @examples
#' # Get EF colors by sport
#' colors <- athlytics_colors_ef()
#' colors$run  # Navy for running
#'
#' @export
athlytics_colors_ef <- function() {
  list(
    run = "#3C5488",      # Navy
    ride = "#F39B7F",     # Coral
    swim = "#4DBBD5",     # Cyan
    other = "#8491B4"     # Slate
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

#' Apply Color Palette to ggplot
#'
#' Helper function to apply Athlytics color palettes to existing plots.
#'
#' @param palette_name Name of palette: "nature", "academic", "vibrant", "science", or "cell"
#' @param type Either "color" or "fill"
#'
#' @return A ggplot2 scale object (scale_color_manual or scale_fill_manual)
#'
#' @examples
#' # Apply nature palette to plot
#' ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   ggplot2::geom_point() +
#'   scale_athlytics("nature", "color")
#'
#' @export
scale_athlytics <- function(palette_name = "nature", type = "color") {
  palette_func <- switch(
    palette_name,
    nature = athlytics_palette_nature,
    academic = athlytics_palette_academic,
    vibrant = athlytics_palette_vibrant,
    science = athlytics_palette_science,
    cell = athlytics_palette_cell,
    athlytics_palette_nature  # default
  )
  
  colors <- palette_func()
  
  if (type == "color") {
    ggplot2::scale_color_manual(values = colors)
  } else {
    ggplot2::scale_fill_manual(values = colors)
  }
}


