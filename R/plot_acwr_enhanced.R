# R/plot_acwr_enhanced.R

#' Enhanced ACWR Plot with Confidence Bands and Reference
#'
#' Creates a comprehensive ACWR visualization with optional confidence bands
#' and cohort reference percentiles.
#'
#' @param acwr_data A data frame from `calculate_acwr_ewma()` containing ACWR values.
#' @param reference_data Optional. A data frame from `cohort_reference()` for
#'   adding cohort reference bands.
#' @param show_ci Logical. Whether to show confidence bands (if available in data).
#'   Default TRUE.
#' @param show_reference Logical. Whether to show cohort reference bands (if provided).
#'   Default TRUE.
#' @param reference_bands Which reference bands to show. Default c("p25_p75", "p05_p95", "p50").
#' @param highlight_zones Logical. Whether to highlight ACWR risk zones. Default TRUE.
#' @param title Plot title. Default NULL (auto-generated).
#' @param subtitle Plot subtitle. Default NULL (auto-generated).
#' @param method_label Optional label for the method used (e.g., "RA", "EWMA"). Default NULL.
#'
#' @return A ggplot object.
#'
#' @details
#' This enhanced plot function combines multiple visualization layers:
#' \itemize{
#'   \item Risk zone shading (sweet spot: 0.8-1.3, caution: 1.3-1.5, high risk: >1.5)
#'   \item Cohort reference percentile bands (if provided)
#'   \item Bootstrap confidence bands (if available in data)
#'   \item Individual ACWR trend line
#' }
#'
#' The layering order (bottom to top):
#' 1. Risk zones (background)
#' 2. Cohort reference bands (P5-P95, then P25-P75)
#' 3. Confidence intervals (individual uncertainty)
#' 4. ACWR line (individual trend)
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_hline labs
#'   scale_x_date theme_minimal theme element_text
#' @importFrom dplyr %>% filter
#' @importFrom tidyr pivot_wider
#' @export
#'
#' @examples
#' # Example using sample data
#' data("athlytics_sample_acwr", package = "Athlytics")
#' if (!is.null(athlytics_sample_acwr) && nrow(athlytics_sample_acwr) > 0) {
#'   p <- plot_acwr_enhanced(athlytics_sample_acwr, show_ci = FALSE)
#'   print(p)
#' }
#'
#' \dontrun{
#' # Load activities
#' activities <- load_local_activities("export.zip")
#'
#' # Calculate ACWR with EWMA and confidence bands
#' acwr <- calculate_acwr_ewma(
#'   activities,
#'   method = "ewma",
#'   ci = TRUE,
#'   B = 200
#' )
#'
#' # Basic enhanced plot
#' plot_acwr_enhanced(acwr)
#'
#' # With cohort reference
#' reference <- cohort_reference(cohort_data, metric = "acwr_smooth")
#' plot_acwr_enhanced(acwr, reference_data = reference)
#' }
plot_acwr_enhanced <- function(acwr_data,
                              reference_data = NULL,
                              show_ci = TRUE,
                              show_reference = TRUE,
                              reference_bands = c("p25_p75", "p05_p95", "p50"),
                              highlight_zones = TRUE,
                              title = NULL,
                              subtitle = NULL,
                              method_label = NULL) {
  
  # --- Input Validation ---
  if (!is.data.frame(acwr_data)) {
    stop("`acwr_data` must be a data frame from calculate_acwr_ewma().")
  }
  
  required_cols <- c("date", "acwr_smooth")
  if (!all(required_cols %in% colnames(acwr_data))) {
    stop("acwr_data must contain columns: date, acwr_smooth")
  }
  
  # Check for CI columns
  has_ci <- all(c("acwr_lower", "acwr_upper") %in% colnames(acwr_data))
  if (show_ci && !has_ci) {
    message("Confidence interval columns not found. Setting show_ci = FALSE.")
    show_ci <- FALSE
  }
  
  # Check for reference data
  if (show_reference && is.null(reference_data)) {
    message("No reference data provided. Setting show_reference = FALSE.")
    show_reference <- FALSE
  }
  
  # --- Create Base Plot ---
  p <- ggplot2::ggplot()
  
  # --- Layer 1: Risk Zones (if enabled) ---
  if (highlight_zones) {
    sweet_spot_min <- 0.8
    sweet_spot_max <- 1.3
    high_risk_min <- 1.5
    
    # Determine y-axis range for zones
    y_max <- max(acwr_data$acwr_smooth, na.rm = TRUE)
    if (has_ci && show_ci) {
      y_max <- max(c(y_max, acwr_data$acwr_upper), na.rm = TRUE)
    }
    y_max <- max(y_max, high_risk_min + 0.2)
    
    # Add zone ribbons
    date_range <- range(acwr_data$date, na.rm = TRUE)
    
    p <- p +
      # High Risk Zone (> 1.5)
      ggplot2::annotate("rect", 
                       xmin = date_range[1], xmax = date_range[2],
                       ymin = high_risk_min, ymax = y_max,
                       fill = "red", alpha = 0.1) +
      # Caution Zone (1.3 - 1.5)
      ggplot2::annotate("rect",
                       xmin = date_range[1], xmax = date_range[2],
                       ymin = sweet_spot_max, ymax = high_risk_min,
                       fill = "orange", alpha = 0.1) +
      # Sweet Spot (0.8 - 1.3)
      ggplot2::annotate("rect",
                       xmin = date_range[1], xmax = date_range[2],
                       ymin = sweet_spot_min, ymax = sweet_spot_max,
                       fill = "green", alpha = 0.1) +
      # Low Load (< 0.8)
      ggplot2::annotate("rect",
                       xmin = date_range[1], xmax = date_range[2],
                       ymin = 0, ymax = sweet_spot_min,
                       fill = "lightblue", alpha = 0.1) +
      # Zone reference lines
      ggplot2::geom_hline(yintercept = c(sweet_spot_min, sweet_spot_max, high_risk_min),
                         linetype = "dotted", color = "grey40", linewidth = 0.5)
  }
  
  # --- Layer 2: Cohort Reference Bands (if provided) ---
  if (show_reference && !is.null(reference_data)) {
    # Pivot reference to wide format
    ref_wide <- reference_data %>%
      dplyr::select(.data$date, .data$percentile, .data$value) %>%
      tidyr::pivot_wider(names_from = .data$percentile, values_from = .data$value)
    
    # Add P5-P95 band (outermost)
    if ("p05_p95" %in% reference_bands && all(c("p05", "p95") %in% colnames(ref_wide))) {
      p <- p + ggplot2::geom_ribbon(
        data = ref_wide,
        ggplot2::aes(x = .data$date, ymin = .data$p05, ymax = .data$p95),
        fill = "#3B528BFF", alpha = 0.15
      )
    }
    
    # Add P25-P75 band (inner)
    if ("p25_p75" %in% reference_bands && all(c("p25", "p75") %in% colnames(ref_wide))) {
      p <- p + ggplot2::geom_ribbon(
        data = ref_wide,
        ggplot2::aes(x = .data$date, ymin = .data$p25, ymax = .data$p75),
        fill = "#440154FF", alpha = 0.25
      )
    }
    
    # Add P50 line (median)
    if ("p50" %in% reference_bands && "p50" %in% colnames(ref_wide)) {
      p <- p + ggplot2::geom_line(
        data = ref_wide,
        ggplot2::aes(x = .data$date, y = .data$p50),
        color = "#21908CFF", linetype = "dashed", linewidth = 0.8
      )
    }
  }
  
  # --- Layer 3: Confidence Bands (if available) ---
  if (show_ci && has_ci) {
    p <- p + ggplot2::geom_ribbon(
      data = acwr_data,
      ggplot2::aes(x = .data$date, ymin = .data$acwr_lower, ymax = .data$acwr_upper),
      fill = "steelblue", alpha = 0.3
    )
  }
  
  # --- Layer 4: Individual ACWR Line ---
  p <- p + ggplot2::geom_line(
    data = acwr_data,
    ggplot2::aes(x = .data$date, y = .data$acwr_smooth),
    color = "#E64B35", linewidth = 2, alpha = 0.9
  )
  
  # --- Labels and Theme ---
  plot_title <- title %||% "Acute:Chronic Workload Ratio (ACWR)"
  
  # Auto-generate subtitle
  if (is.null(subtitle)) {
    subtitle_parts <- c()
    if (!is.null(method_label)) {
      subtitle_parts <- c(subtitle_parts, paste("Method:", method_label))
    }
    if (show_ci && has_ci) {
      subtitle_parts <- c(subtitle_parts, "with 95% CI")
    }
    if (show_reference && !is.null(reference_data)) {
      subtitle_parts <- c(subtitle_parts, "vs cohort reference")
    }
    subtitle <- if (length(subtitle_parts) > 0) {
      paste(subtitle_parts, collapse = " | ")
    } else {
      NULL
    }
  }
  
  p <- p +
    ggplot2::labs(
      title = plot_title,
      subtitle = subtitle,
      x = "Date",
      y = "ACWR (Smoothed)",
      caption = if (highlight_zones) {
        "Zones: Green = Sweet Spot (0.8-1.3) | Orange = Caution | Red = High Risk (>1.5)"
      } else NULL
    ) +
    ggplot2::scale_x_date(
      date_breaks = "3 months",
      labels = function(x) {
        months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        paste(months[as.integer(format(x, "%m"))], format(x, "%Y"))
      }
    ) +
    theme_athlytics() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}


#' Compare RA and EWMA Methods Side-by-Side
#'
#' Creates a faceted plot comparing Rolling Average and EWMA ACWR calculations.
#'
#' @param acwr_ra A data frame from `calculate_acwr_ewma(..., method = "ra")`.
#' @param acwr_ewma A data frame from `calculate_acwr_ewma(..., method = "ewma")`.
#' @param title Plot title. Default "ACWR Method Comparison: RA vs EWMA".
#'
#' @return A ggplot object with faceted comparison.
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs theme_minimal
#' @importFrom dplyr bind_rows mutate
#' @export
#'
#' @examples
#' # Example using sample data
#' data("athlytics_sample_acwr", package = "Athlytics")
#' if (!is.null(athlytics_sample_acwr) && nrow(athlytics_sample_acwr) > 0) {
#'   # Create two versions for comparison (simulate RA vs EWMA)
#'   acwr_ra <- athlytics_sample_acwr
#'   acwr_ewma <- athlytics_sample_acwr
#'   acwr_ewma$acwr_smooth <- acwr_ewma$acwr_smooth * runif(nrow(acwr_ewma), 0.95, 1.05)
#'   
#'   p <- plot_acwr_comparison(acwr_ra, acwr_ewma)
#'   print(p)
#' }
#'
#' \dontrun{
#' activities <- load_local_activities("export.zip")
#'
#' acwr_ra <- calculate_acwr_ewma(activities, method = "ra")
#' acwr_ewma <- calculate_acwr_ewma(activities, method = "ewma")
#'
#' plot_acwr_comparison(acwr_ra, acwr_ewma)
#' }
plot_acwr_comparison <- function(acwr_ra,
                                acwr_ewma,
                                title = "ACWR Method Comparison: RA vs EWMA") {
  
  # Combine data with method labels
  combined <- dplyr::bind_rows(
    acwr_ra %>% dplyr::mutate(method = "Rolling Average (RA)"),
    acwr_ewma %>% dplyr::mutate(method = "EWMA")
  )
  
  # Create faceted plot
  p <- ggplot2::ggplot(combined, ggplot2::aes(x = .data$date, y = .data$acwr_smooth, color = .data$method)) +
    ggplot2::geom_hline(yintercept = c(0.8, 1.3, 1.5), 
                       linetype = "dotted", color = "gray50", alpha = 0.5) +
    ggplot2::geom_line(linewidth = 1.8, alpha = 0.9) +
    ggplot2::scale_color_manual(values = c("Rolling Average (RA)" = "#4DBBD5", "EWMA" = "#E64B35")) +
    ggplot2::facet_wrap(~.data$method, ncol = 1) +
    ggplot2::labs(
      title = title,
      subtitle = "Dotted lines: 0.8 (low load) | 1.3 (sweet spot max) | 1.5 (high risk)",
      x = "Date",
      y = "ACWR (Smoothed)"
    ) +
    ggplot2::scale_x_date(
      date_breaks = "3 months",
      labels = function(x) {
        months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        paste(months[as.integer(format(x, "%m"))], format(x, "%Y"))
      }
    ) +
    theme_athlytics() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  
  return(p)
}


