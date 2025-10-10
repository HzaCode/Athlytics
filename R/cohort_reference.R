# R/cohort_reference.R

#' Calculate Cohort Reference Percentiles
#'
#' Calculates reference percentiles for a metric across a cohort of athletes,
#' stratified by specified grouping variables (e.g., sport, sex, age band).
#'
#' @param data A data frame containing metric values for multiple athletes.
#'   Must include columns: `date`, `athlete_id`, and the metric column.
#' @param metric Name of the metric column to calculate percentiles for
#'   (e.g., "acwr", "acwr_smooth", "ef", "decoupling"). Default "acwr_smooth".
#' @param by Character vector of grouping variables. Options: "sport", "sex", 
#'   "age_band", "athlete_id". Default c("sport").
#' @param probs Numeric vector of probabilities for percentiles (0-1).
#'   Default c(0.05, 0.25, 0.50, 0.75, 0.95) for 5th, 25th, 50th, 75th, 95th percentiles.
#' @param min_athletes Minimum number of athletes required per group to calculate
#'   valid percentiles. Default 5.
#' @param date_col Name of the date column. Default "date".
#'
#' @return A long-format data frame with columns:
#'   \describe{
#'     \item{date}{Date}
#'     \item{...}{Grouping variables (as specified in `by`)}
#'     \item{percentile}{Percentile label (e.g., "p05", "p25", "p50", "p75", "p95")}
#'     \item{value}{Metric value at that percentile}
#'     \item{n_athletes}{Number of athletes contributing to this percentile}
#'   }
#'
#' @details
#' This function creates cohort-level reference bands for comparing individual
#' athlete metrics to their peers. Common use cases:
#' \itemize{
#'   \item Compare an athlete's ACWR trend to team averages
#'   \item Identify outliers (athletes outside P5-P95 range)
#'   \item Track team-wide trends over time
#' }
#'
#' **Important**: Percentile bands represent **population variability**, not
#' statistical confidence intervals for individual values.
#'
#' @importFrom dplyr group_by summarise filter mutate %>% ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom stats quantile setNames
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' # Load activities for multiple athletes
#' athlete1 <- load_local_activities("athlete1_export.zip") %>%
#'   mutate(athlete_id = "athlete1")
#' athlete2 <- load_local_activities("athlete2_export.zip") %>%
#'   mutate(athlete_id = "athlete2")
#' athlete3 <- load_local_activities("athlete3_export.zip") %>%
#'   mutate(athlete_id = "athlete3")
#'
#' # Combine data
#' cohort_data <- bind_rows(athlete1, athlete2, athlete3)
#'
#' # Calculate ACWR for each athlete
#' cohort_acwr <- cohort_data %>%
#'   group_by(athlete_id) %>%
#'   do(calculate_acwr_ewma(.))
#'
#' # Calculate reference percentiles
#' reference <- cohort_reference(
#'   cohort_acwr,
#'   metric = "acwr_smooth",
#'   by = c("sport"),
#'   probs = c(0.05, 0.25, 0.5, 0.75, 0.95)
#' )
#'
#' # Plot individual against cohort
#' plot_with_reference(
#'   individual = cohort_acwr %>% filter(athlete_id == "athlete1"),
#'   reference = reference
#' )
#' }
cohort_reference <- function(data,
                            metric = "acwr_smooth",
                            by = c("sport"),
                            probs = c(0.05, 0.25, 0.50, 0.75, 0.95),
                            min_athletes = 5,
                            date_col = "date") {
  
  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  
  if (nrow(data) == 0) {
    stop("`data` is empty.")
  }
  
  required_cols <- c(date_col, metric)
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Check if grouping variables exist
  if (length(by) > 0) {
    missing_by <- setdiff(by, colnames(data))
    if (length(missing_by) > 0) {
      warning(sprintf("Grouping variable(s) not found: %s. Will proceed without them.",
                     paste(missing_by, collapse = ", ")))
      by <- setdiff(by, missing_by)
    }
  }
  
  # Ensure athlete_id exists if not in grouping
  if (!"athlete_id" %in% colnames(data) && !"athlete_id" %in% by) {
    warning("`athlete_id` column not found. Assuming single athlete or unable to count distinct athletes.")
    data$athlete_id <- "unknown"
  }
  
  # --- Calculate Percentiles ---
  message(sprintf("Calculating percentiles for metric '%s' grouped by: %s",
                  metric, paste(by, collapse = ", ")))
  
  if (length(by) == 0) {
    # No grouping - calculate overall percentiles by date
    grouping_vars <- date_col
  } else {
    grouping_vars <- c(date_col, by)
  }
  
  # Create dynamic grouping
  reference_data <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) %>%
    dplyr::summarise(
      n_athletes = dplyr::n_distinct(.data$athlete_id, na.rm = TRUE),
      !!!stats::setNames(
        lapply(probs, function(p) {
          rlang::expr(stats::quantile(.data[[!!metric]], probs = !!p, na.rm = TRUE))
        }),
        paste0("p", sprintf("%02d", probs * 100))
      ),
      .groups = 'drop'
    )
  
  # Filter by minimum athletes
  reference_data <- reference_data %>%
    dplyr::filter(.data$n_athletes >= min_athletes)
  
  if (nrow(reference_data) == 0) {
    stop(sprintf("No groups have at least %d athletes. Cannot calculate valid percentiles.", 
                 min_athletes))
  }
  
  # Pivot to long format
  percentile_cols <- paste0("p", sprintf("%02d", probs * 100))
  reference_long <- reference_data %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(percentile_cols),
      names_to = "percentile",
      values_to = "value"
    )
  
  message(sprintf("Reference calculated for %d date-group combinations.", 
                  nrow(reference_data)))
  
  return(reference_long)
}


#' Add Cohort Reference Bands to Existing Plot
#'
#' Adds percentile reference bands from a cohort to an individual's metric plot.
#'
#' @param p A ggplot object (typically from plot_acwr or similar).
#' @param reference_data A data frame from `cohort_reference()`.
#' @param bands Character vector specifying which bands to plot. Options:
#'   "p25_p75" (inner quartiles), "p05_p95" (outer 5-95 range), "p50" (median).
#'   Default c("p25_p75", "p05_p95", "p50").
#' @param alpha Transparency for reference bands (0-1). Default 0.15.
#' @param colors Named list of colors for bands. Default uses viridis colors.
#'
#' @return A ggplot object with added reference bands.
#'
#' @importFrom ggplot2 geom_ribbon geom_line aes
#' @importFrom dplyr filter %>%
#' @importFrom tidyr pivot_wider
#' @export
#'
#' @examples
#' \dontrun{
#' # Create base plot
#' p <- plot_acwr(acwr_df = individual_acwr)
#'
#' # Add reference bands
#' p_with_ref <- add_reference_bands(p, reference_data = cohort_ref)
#' print(p_with_ref)
#' }
add_reference_bands <- function(p,
                               reference_data,
                               bands = c("p25_p75", "p05_p95", "p50"),
                               alpha = 0.15,
                               colors = list(
                                 p25_p75 = "#440154FF",  # viridis dark purple
                                 p05_p95 = "#3B528BFF",  # viridis blue
                                 p50 = "#21908CFF"       # viridis teal
                               )) {
  
  if (!inherits(p, "ggplot")) {
    stop("`p` must be a ggplot object.")
  }
  
  if (!is.data.frame(reference_data)) {
    stop("`reference_data` must be a data frame from cohort_reference().")
  }
  
  # Pivot reference data to wide format for plotting
  ref_wide <- reference_data %>%
    dplyr::select(.data$date, .data$percentile, .data$value) %>%
    tidyr::pivot_wider(names_from = .data$percentile, values_from = .data$value)
  
  # Add bands in order (outermost to innermost)
  if ("p05_p95" %in% bands && all(c("p05", "p95") %in% colnames(ref_wide))) {
    p <- p + ggplot2::geom_ribbon(
      data = ref_wide,
      ggplot2::aes(x = .data$date, ymin = .data$p05, ymax = .data$p95),
      fill = colors$p05_p95,
      alpha = alpha
    )
  }
  
  if ("p25_p75" %in% bands && all(c("p25", "p75") %in% colnames(ref_wide))) {
    p <- p + ggplot2::geom_ribbon(
      data = ref_wide,
      ggplot2::aes(x = .data$date, ymin = .data$p25, ymax = .data$p75),
      fill = colors$p25_p75,
      alpha = alpha * 1.5
    )
  }
  
  if ("p50" %in% bands && "p50" %in% colnames(ref_wide)) {
    p <- p + ggplot2::geom_line(
      data = ref_wide,
      ggplot2::aes(x = .data$date, y = .data$p50),
      color = colors$p50,
      linetype = "dashed",
      linewidth = 0.8
    )
  }
  
  return(p)
}


#' Plot Individual Metric with Cohort Reference
#'
#' Creates a complete plot showing an individual's metric trend with cohort
#' reference percentile bands.
#'
#' @param individual A data frame with individual athlete data (from calculate_acwr, etc.)
#' @param reference A data frame from `cohort_reference()`.
#' @param metric Name of the metric to plot. Default "acwr_smooth".
#' @param date_col Name of the date column. Default "date".
#' @param title Plot title. Default NULL (auto-generated).
#' @param bands Which reference bands to show. Default c("p25_p75", "p05_p95", "p50").
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal scale_x_date
#' @importFrom dplyr %>%
#' @importFrom rlang %||% .data
#' @export
#'
#' @examples
#' \dontrun{
#' plot_with_reference(
#'   individual = athlete_acwr,
#'   reference = cohort_ref,
#'   metric = "acwr_smooth"
#' )
#' }
plot_with_reference <- function(individual,
                               reference,
                               metric = "acwr_smooth",
                               date_col = "date",
                               title = NULL,
                               bands = c("p25_p75", "p05_p95", "p50")) {
  
  if (!is.data.frame(individual) || !is.data.frame(reference)) {
    stop("Both `individual` and `reference` must be data frames.")
  }
  
  if (!metric %in% colnames(individual)) {
    stop(sprintf("Metric '%s' not found in individual data.", metric))
  }
  
  # Pivot reference to wide
  ref_wide <- reference %>%
    dplyr::select(.data$date, .data$percentile, .data$value) %>%
    tidyr::pivot_wider(names_from = .data$percentile, values_from = .data$value)
  
  # Create base plot with reference bands
  p <- ggplot2::ggplot()
  
  # Add reference bands (outermost first)
  if ("p05_p95" %in% bands && all(c("p05", "p95") %in% colnames(ref_wide))) {
    p <- p + ggplot2::geom_ribbon(
      data = ref_wide,
      ggplot2::aes(x = .data$date, ymin = .data$p05, ymax = .data$p95),
      fill = "#3B528BFF", alpha = 0.15
    ) +
    ggplot2::annotate("text", x = min(ref_wide$date), y = max(ref_wide$p95, na.rm = TRUE),
                     label = "P5-P95", hjust = 0, vjust = 1, size = 3, color = "#3B528BFF", alpha = 0.7)
  }
  
  if ("p25_p75" %in% bands && all(c("p25", "p75") %in% colnames(ref_wide))) {
    p <- p + ggplot2::geom_ribbon(
      data = ref_wide,
      ggplot2::aes(x = .data$date, ymin = .data$p25, ymax = .data$p75),
      fill = "#440154FF", alpha = 0.25
    ) +
    ggplot2::annotate("text", x = min(ref_wide$date), y = max(ref_wide$p75, na.rm = TRUE),
                     label = "P25-P75", hjust = 0, vjust = 1, size = 3, color = "#440154FF", alpha = 0.7)
  }
  
  if ("p50" %in% bands && "p50" %in% colnames(ref_wide)) {
    p <- p + ggplot2::geom_line(
      data = ref_wide,
      ggplot2::aes(x = .data$date, y = .data$p50),
      color = "#21908CFF", linetype = "dashed", linewidth = 0.8
    )
  }
  
  # Add individual line on top
  p <- p + ggplot2::geom_line(
    data = individual,
    ggplot2::aes(x = .data[[date_col]], y = .data[[metric]]),
    color = "black", linewidth = 1.2
  )
  
  # Formatting
  plot_title <- title %||% sprintf("%s: Individual vs Cohort", tools::toTitleCase(metric))
  
  p <- p +
    ggplot2::labs(
      title = plot_title,
      subtitle = "Black line = Individual | Shaded areas = Cohort percentiles",
      x = "Date",
      y = tools::toTitleCase(metric)
    ) +
    ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(face = "bold")
    )
  
  return(p)
}

