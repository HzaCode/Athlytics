# R/calculate_acwr_ewma.R

#' Calculate ACWR using EWMA Method with Confidence Bands
#'
#' Calculates the Acute:Chronic Workload Ratio (ACWR) using Exponentially
#' Weighted Moving Average (EWMA) with optional bootstrap confidence bands.
#'
#' @param activities_data A data frame of activities from `load_local_activities()`.
#' @param activity_type Optional. Filter activities by type. Default NULL includes all.
#' @param load_metric Method for calculating daily load. Default "duration_mins".
#' @param method ACWR calculation method: "ra" (rolling average) or "ewma". Default "ra".
#' @param acute_period Days for acute window (for RA method). Default 7.
#' @param chronic_period Days for chronic window (for RA method). Default 28.
#' @param half_life_acute Half-life for acute EWMA in days. Default 3.5.
#' @param half_life_chronic Half-life for chronic EWMA in days. Default 14.
#' @param start_date Optional. Analysis start date. Defaults to one year ago.
#' @param end_date Optional. Analysis end date. Defaults to today.
#' @param user_ftp Required if `load_metric = "tss"`.
#' @param user_max_hr Required if `load_metric = "hrss"`.
#' @param user_resting_hr Required if `load_metric = "hrss"`.
#' @param smoothing_period Days for smoothing ACWR. Default 7.
#' @param ci Logical. Whether to calculate confidence bands (EWMA only). Default FALSE.
#' @param B Number of bootstrap iterations (if ci = TRUE). Default 200.
#' @param block_len Block length for moving-block bootstrap (days). Default 7.
#' @param conf_level Confidence level (0-1). Default 0.95 (95\\% CI).
#'
#' @return A data frame with columns: `date`, `atl`, `ctl`, `acwr`, `acwr_smooth`,
#'   and if `ci = TRUE` and `method = "ewma"`: `acwr_lower`, `acwr_upper`.
#'
#' @details
#' This function extends the basic ACWR calculation with two methods:
#' \itemize{
#'   \item **RA (Rolling Average)**: Traditional rolling mean approach (default).
#'   \item **EWMA (Exponentially Weighted Moving Average)**: Uses exponential decay
#'     with configurable half-lives. More responsive to recent changes.
#' }
#'
#' **EWMA Formula**: The smoothing parameter alpha is calculated from half-life: 
#' `alpha = ln(2) / half_life`. The EWMA update is: `E_t = alpha * L_t + (1-alpha) * E_{t-1}`
#' where L_t is daily load and E_t is the exponentially weighted average.
#'
#' **Confidence Bands**: When `ci = TRUE` and `method = "ewma"`, uses **moving-block
#' bootstrap** to estimate uncertainty. The daily load sequence is resampled in
#' weekly blocks (preserving within-week correlation), ACWR is recalculated,
#' and percentiles form the confidence bands. This accounts for temporal correlation
#' in training load patterns.
#'
#' @importFrom dplyr filter select mutate group_by summarise arrange %>% left_join coalesce
#' @importFrom lubridate as_date days
#' @importFrom stats quantile
#'
#' @examples
#' \dontrun{
#' # Load local activities
#' activities <- load_local_activities("export_12345678.zip")
#'
#' # Calculate ACWR using Rolling Average (RA)
#' acwr_ra <- calculate_acwr_ewma(activities, method = "ra")
#'
#' # Calculate ACWR using EWMA with confidence bands
#' acwr_ewma <- calculate_acwr_ewma(
#'   activities,
#'   method = "ewma",
#'   half_life_acute = 3.5,
#'   half_life_chronic = 14,
#'   ci = TRUE,
#'   B = 200
#' )
#'
#' # Compare both methods
#' head(acwr_ewma)
#' }
#' @export
calculate_acwr_ewma <- function(activities_data,
                               activity_type = NULL,
                               load_metric = "duration_mins",
                               method = c("ra", "ewma"),
                               acute_period = 7,
                               chronic_period = 28,
                               half_life_acute = 3.5,
                               half_life_chronic = 14,
                               start_date = NULL,
                               end_date = NULL,
                               user_ftp = NULL,
                               user_max_hr = NULL,
                               user_resting_hr = NULL,
                               smoothing_period = 7,
                               ci = FALSE,
                               B = 200,
                               block_len = 7,
                               conf_level = 0.95) {
  
  # --- Match method argument ---
  method <- match.arg(method)
  
  # --- Input Validation ---
  if (missing(activities_data) || is.null(activities_data)) {
    stop("`activities_data` must be provided.")
  }
  
  if (!is.data.frame(activities_data)) {
    stop("`activities_data` must be a data frame.")
  }
  
  if (method == "ewma") {
    if (!is.numeric(half_life_acute) || half_life_acute <= 0) {
      stop("`half_life_acute` must be a positive number.")
    }
    if (!is.numeric(half_life_chronic) || half_life_chronic <= 0) {
      stop("`half_life_chronic` must be a positive number.")
    }
    if (half_life_acute >= half_life_chronic) {
      stop("`half_life_acute` must be less than `half_life_chronic`.")
    }
  } else {
    if (!is.numeric(acute_period) || acute_period <= 0) {
      stop("`acute_period` must be a positive integer.")
    }
    if (!is.numeric(chronic_period) || chronic_period <= 0) {
      stop("`chronic_period` must be a positive integer.")
    }
    if (acute_period >= chronic_period) {
      stop("`acute_period` must be less than `chronic_period`.")
    }
  }
  
  if (ci && method == "ra") {
    warning("Confidence bands are only available for EWMA method. Setting ci = FALSE.")
    ci <- FALSE
  }
  
  # --- Date Handling ---
  analysis_end_date <- tryCatch(lubridate::as_date(end_date %||% Sys.Date()), 
                                 error = function(e) Sys.Date())
  analysis_start_date <- tryCatch(lubridate::as_date(start_date %||% (analysis_end_date - lubridate::days(365))), 
                                   error = function(e) analysis_end_date - lubridate::days(365))
  
  if (analysis_start_date >= analysis_end_date) {
    stop("start_date must be before end_date.")
  }
  
  # Validate load_metric and required parameters
  valid_load_metrics <- c("duration_mins", "distance_km", "elapsed_time_mins", "tss", "hrss", "elevation_gain_m")
  if (!load_metric %in% valid_load_metrics) {
    stop("Invalid `load_metric`. Choose from: ", paste(valid_load_metrics, collapse = ", "))
  }
  
  if (load_metric == "tss" && is.null(user_ftp)) {
    stop("`user_ftp` is required when `load_metric` is 'tss'.")
  }
  
  if (load_metric == "hrss" && (is.null(user_max_hr) || is.null(user_resting_hr))) {
    stop("`user_max_hr` and `user_resting_hr` are required when `load_metric` is 'hrss'.")
  }
  
  # Force explicit activity_type specification to prevent mixing incompatible sports
  if (is.null(activity_type) || length(activity_type) == 0) {
    stop("`activity_type` must be explicitly specified (e.g., 'Run' or 'Ride'). ",
         "Mixing different activity types can lead to incompatible load metrics. ",
         "Please specify the activity type(s) you want to analyze.")
  }
  
  message(sprintf("Calculating ACWR (%s) from %s to %s.", 
                  toupper(method), analysis_start_date, analysis_end_date))
  message(sprintf("Load metric: %s, Activity types: %s", 
                  load_metric, paste(activity_type %||% "All", collapse = ", ")))
  
  # --- Get Daily Load (reuse logic from calculate_acwr) ---
  fetch_start_buffer_days <- if (method == "ra") chronic_period else ceiling(4 * half_life_chronic)
  fetch_start_date <- analysis_start_date - lubridate::days(fetch_start_buffer_days)
  
  # Filter activities
  activities_df_filtered <- activities_data %>%
    dplyr::filter(.data$date >= fetch_start_date & .data$date <= analysis_end_date)
  
  if (!is.null(activity_type)) {
    activities_df_filtered <- activities_df_filtered %>%
      dplyr::filter(.data$type %in% activity_type)
  }
  
  message(sprintf("Processing %d activities...", nrow(activities_df_filtered)))
  
  if (nrow(activities_df_filtered) == 0) {
    stop("No activities found for the specified criteria.")
  }
  
  # Calculate daily load using internal helper
  daily_load_df <- calculate_daily_load_internal(activities_df_filtered, load_metric, 
                                                  user_ftp, user_max_hr, user_resting_hr)
  
  daily_load_summary <- daily_load_df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(daily_load = sum(load, na.rm = TRUE), .groups = 'drop')
  
  # Create complete time series
  all_dates_sequence <- seq(fetch_start_date, analysis_end_date, by = "day")
  daily_load_complete <- dplyr::tibble(date = all_dates_sequence) %>%
    dplyr::left_join(daily_load_summary, by = "date") %>%
    dplyr::mutate(daily_load = dplyr::coalesce(.data$daily_load, 0)) %>%
    dplyr::arrange(.data$date)
  
  # --- Calculate ACWR based on method ---
  if (method == "ra") {
    acwr_data <- calculate_acwr_ra_internal(daily_load_complete, acute_period, 
                                             chronic_period, smoothing_period,
                                             analysis_start_date, analysis_end_date)
  } else {
    acwr_data <- calculate_acwr_ewma_internal(daily_load_complete, half_life_acute, 
                                               half_life_chronic, smoothing_period,
                                               analysis_start_date, analysis_end_date,
                                               ci, B, block_len, conf_level)
  }
  
  message("Calculation complete.")
  return(acwr_data)
}


#' Internal: Calculate ACWR using Rolling Average
#' @keywords internal
#' @noRd
calculate_acwr_ra_internal <- function(daily_load_complete, acute_period, chronic_period,
                                      smoothing_period, start_date, end_date) {
  acwr_data <- daily_load_complete %>%
    dplyr::mutate(
      daily_load = as.numeric(.data$daily_load),
      acute_load = zoo::rollmean(.data$daily_load, k = acute_period, fill = NA, align = "right"),
      chronic_load = zoo::rollmean(.data$daily_load, k = chronic_period, fill = NA, align = "right"),
      acwr = ifelse(!is.na(.data$chronic_load) & .data$chronic_load > 0.01,
                    .data$acute_load / .data$chronic_load, NA),
      acwr_smooth = zoo::rollmean(.data$acwr, k = smoothing_period, align = "right", fill = NA)
    ) %>%
    dplyr::filter(.data$date >= start_date & .data$date <= end_date) %>%
    dplyr::select(.data$date, atl = .data$acute_load, ctl = .data$chronic_load, 
                  .data$acwr, .data$acwr_smooth)
  
  return(acwr_data)
}


#' Internal: Calculate ACWR using EWMA
#' @keywords internal
#' @noRd
calculate_acwr_ewma_internal <- function(daily_load_complete, half_life_acute, half_life_chronic,
                                        smoothing_period, start_date, end_date,
                                        ci, B, block_len, conf_level) {
  
  # Calculate alpha from half-life: α = ln(2) / half_life
  alpha_acute <- log(2) / half_life_acute
  alpha_chronic <- log(2) / half_life_chronic
  
  # Calculate EWMA loads
  loads <- daily_load_complete$daily_load
  n <- length(loads)
  
  acute_load <- numeric(n)
  chronic_load <- numeric(n)
  
  acute_load[1] <- loads[1]
  chronic_load[1] <- loads[1]
  
  for (i in 2:n) {
    acute_load[i] <- alpha_acute * loads[i] + (1 - alpha_acute) * acute_load[i-1]
    chronic_load[i] <- alpha_chronic * loads[i] + (1 - alpha_chronic) * chronic_load[i-1]
  }
  
  # Calculate ACWR
  acwr <- ifelse(chronic_load > 0.01, acute_load / chronic_load, NA)
  
  # Smooth ACWR
  acwr_smooth <- zoo::rollmean(acwr, k = smoothing_period, align = "right", fill = NA)
  
  # Build base result
  acwr_data <- data.frame(
    date = daily_load_complete$date,
    atl = acute_load,
    ctl = chronic_load,
    acwr = acwr,
    acwr_smooth = acwr_smooth
  ) %>%
    dplyr::filter(.data$date >= start_date & .data$date <= end_date)
  
  # --- Bootstrap Confidence Bands ---
  if (ci) {
    message(sprintf("Calculating %d%% confidence bands using %d bootstrap iterations...", 
                    conf_level * 100, B))
    
    ci_bounds <- bootstrap_acwr_ci(loads, alpha_acute, alpha_chronic, 
                                   smoothing_period, B, block_len, conf_level)
    
    # Trim to analysis period
    start_idx <- which(daily_load_complete$date == start_date)[1]
    end_idx <- which(daily_load_complete$date == end_date)[1]
    
    if (!is.na(start_idx) && !is.na(end_idx)) {
      acwr_data$acwr_lower <- ci_bounds$lower[start_idx:end_idx]
      acwr_data$acwr_upper <- ci_bounds$upper[start_idx:end_idx]
    } else {
      acwr_data$acwr_lower <- NA
      acwr_data$acwr_upper <- NA
    }
  }
  
  return(acwr_data)
}


#' Internal: Bootstrap Confidence Intervals for EWMA ACWR
#' @keywords internal
#' @noRd
bootstrap_acwr_ci <- function(loads, alpha_acute, alpha_chronic, 
                             smoothing_period, B, block_len, conf_level) {
  
  n <- length(loads)
  n_blocks <- ceiling(n / block_len)
  
  # Store bootstrap ACWR values
  boot_acwr_matrix <- matrix(NA, nrow = n, ncol = B)
  
  for (b in 1:B) {
    # Moving-block bootstrap: sample blocks with replacement
    sampled_blocks <- sample(1:n_blocks, n_blocks, replace = TRUE)
    boot_loads <- numeric(0)
    
    for (block_idx in sampled_blocks) {
      start_pos <- (block_idx - 1) * block_len + 1
      end_pos <- min(block_idx * block_len, n)
      boot_loads <- c(boot_loads, loads[start_pos:end_pos])
    }
    
    # Trim to original length
    boot_loads <- boot_loads[1:n]
    
    # Calculate EWMA for this bootstrap sample
    acute_boot <- numeric(n)
    chronic_boot <- numeric(n)
    acute_boot[1] <- boot_loads[1]
    chronic_boot[1] <- boot_loads[1]
    
    for (i in 2:n) {
      acute_boot[i] <- alpha_acute * boot_loads[i] + (1 - alpha_acute) * acute_boot[i-1]
      chronic_boot[i] <- alpha_chronic * boot_loads[i] + (1 - alpha_chronic) * chronic_boot[i-1]
    }
    
    acwr_boot <- ifelse(chronic_boot > 0.01, acute_boot / chronic_boot, NA)
    acwr_boot_smooth <- zoo::rollmean(acwr_boot, k = smoothing_period, align = "right", fill = NA)
    
    boot_acwr_matrix[, b] <- acwr_boot_smooth
  }
  
  # Calculate percentiles
  alpha_level <- 1 - conf_level
  lower_quantile <- alpha_level / 2
  upper_quantile <- 1 - alpha_level / 2
  
  acwr_lower <- apply(boot_acwr_matrix, 1, function(x) quantile(x, probs = lower_quantile, na.rm = TRUE))
  acwr_upper <- apply(boot_acwr_matrix, 1, function(x) quantile(x, probs = upper_quantile, na.rm = TRUE))
  
  list(lower = acwr_lower, upper = acwr_upper)
}


