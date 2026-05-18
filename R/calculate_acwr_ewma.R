# R/calculate_acwr_ewma.R

#' Calculate ACWR using EWMA Method with Confidence Bands
#'
#' Calculates the Acute:Chronic Workload Ratio (ACWR) using Exponentially
#' Weighted Moving Average (EWMA) with optional bootstrap confidence bands.
#'
#' @param activities_data A data frame of activities from `load_local_activities()`.
#' @param activity_type Required. Filter activities by type, such as `"Run"` or
#'   `"Ride"`. Explicit filtering avoids mixing incompatible sport contexts.
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
#' @param missing_load How to treat training days on which the chosen
#'   `load_metric` could not be computed (`"zero"` for historical
#'   behaviour or `"na"` to keep missing-data training days visibly NA).
#'   See `calculate_acwr()` for details.
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
#' - **RA (Rolling Average)**: Traditional rolling mean approach (default).
#' - **EWMA (Exponentially Weighted Moving Average)**: Uses exponential decay
#'   with configurable half-lives. More responsive to recent changes.
#'
#' **EWMA Formula**: The smoothing parameter alpha is calculated from half-life:
#' `alpha = 1 - exp(-ln(2) / half_life)`. The EWMA update is:
#' `E_t = alpha * L_t + (1-alpha) * E_{t-1}` where L_t is daily load
#' and E_t is the exponentially weighted average.
#'
#' **Confidence Bands**: When `ci = TRUE` and `method = "ewma"`, uses **moving-block
#' bootstrap** to estimate uncertainty. The daily load sequence is resampled in
#' overlapping weekly blocks (preserving within-week correlation), ACWR is recalculated,
#' and percentiles form the confidence bands. This accounts for temporal correlation
#' in training load patterns.
#'
#'
#' @examples
#' # Example using pre-calculated sample data
#' data("sample_acwr", package = "Athlytics")
#' head(sample_acwr)
#'
#' \dontrun{
#' # Full workflow with real data - Load local activities
#' activities <- load_local_activities("export_12345678.zip")
#'
#' # Calculate ACWR using Rolling Average (RA)
#' acwr_ra <- calculate_acwr_ewma(activities, activity_type = "Run", method = "ra")
#'
#' # Calculate ACWR using EWMA with confidence bands
#' acwr_ewma <- calculate_acwr_ewma(
#'   activities,
#'   activity_type = "Run",
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
                                activity_type,
                                load_metric = "duration_mins",
                                method = c("ra", "ewma"),
                                acute_period = 7,
                                chronic_period = 28,
                                half_life_acute = 3.5,
                                half_life_chronic = 14,
                                start_date = NULL,
                                end_date = Sys.Date(),
                                user_ftp = NULL,
                                user_max_hr = NULL,
                                user_resting_hr = NULL,
                                smoothing_period = 7,
                                missing_load = c("zero", "na"),
                                ci = FALSE,
                                B = 200,
                                block_len = 7,
                                conf_level = 0.95) {
  # --- Match method argument ---
  method <- match.arg(method)
  missing_load <- match.arg(missing_load)

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

  if (!is.numeric(smoothing_period) || length(smoothing_period) != 1 ||
    is.na(smoothing_period) || smoothing_period <= 0 ||
    smoothing_period != floor(smoothing_period)) {
    stop("`smoothing_period` must be a positive integer.")
  }

  if (ci && method == "ra") {
    warning("Confidence bands are only available for EWMA method. Setting ci = FALSE.")
    ci <- FALSE
  }

  if (ci) {
    if (!is.numeric(B) || length(B) != 1 || is.na(B) || B < 1 || B != floor(B)) {
      stop("`B` must be a positive integer.")
    }
    if (!is.numeric(block_len) || length(block_len) != 1 ||
      is.na(block_len) || block_len < 1 || block_len != floor(block_len)) {
      stop("`block_len` must be a positive integer.")
    }
    if (!is.numeric(conf_level) || length(conf_level) != 1 ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
      stop("`conf_level` must be between 0 and 1.")
    }
  }

  # --- Date Handling ---
  analysis_end_date <- parse_analysis_date(end_date, default = Sys.Date(), arg_name = "end_date")
  analysis_start_date <- parse_analysis_date(
    start_date,
    default = analysis_end_date - lubridate::days(365),
    arg_name = "start_date"
  )

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
  if (missing(activity_type) || is.null(activity_type) || length(activity_type) == 0) {
    stop(
      "`activity_type` must be explicitly specified (e.g., 'Run' or 'Ride'). ",
      "Mixing different activity types can lead to incompatible load metrics. ",
      "Please specify the activity type(s) you want to analyze."
    )
  }

  athlytics_message(sprintf(
    "Calculating ACWR (%s) from %s to %s.",
    toupper(method), analysis_start_date, analysis_end_date
  ))
  athlytics_message(sprintf(
    "Load metric: %s, Activity types: %s",
    load_metric, paste(activity_type %||% "All", collapse = ", ")
  ))

  # --- Get Daily Load (reuse logic from calculate_acwr) ---
  fetch_start_buffer_days <- if (method == "ra") chronic_period else ceiling(4 * half_life_chronic)
  fetch_start_date <- analysis_start_date - lubridate::days(fetch_start_buffer_days)

  activities_df_scoped <- activity_history_scope(
    activities_data, activity_type, analysis_end_date
  )
  effective_chronic <- if (method == "ra") {
    chronic_period
  } else {
    ceiling(3 * half_life_chronic)
  }
  fetch_start_date <- clip_unknown_prehistory_start(
    scoped_activities = activities_df_scoped,
    fetch_start_date = fetch_start_date,
    analysis_start_date = analysis_start_date,
    analysis_end_date = analysis_end_date,
    baseline_days = effective_chronic,
    metric_label = "ACWR"
  )
  activities_df_filtered <- activities_df_scoped %>%
    dplyr::filter(.data$date >= fetch_start_date)

  athlytics_message(sprintf("Processing %d activities...", nrow(activities_df_filtered)))

  if (nrow(activities_df_filtered) == 0) {
    stop("No activities found for the specified criteria.")
  }

  # Calculate daily load using internal helper
  daily_load_df <- calculate_daily_load_internal(
    activities_df_filtered, load_metric,
    user_ftp, user_max_hr, user_resting_hr
  )

  # Warn once if "zero" mode is silently absorbing data-quality gaps.
  warn_missing_load_absorbed(daily_load_df, missing_load)

  # Aggregate to daily load; preserve NA on days where every activity had a
  # non-computable load so `missing_load = "na"` can propagate it below.
  daily_load_summary <- daily_load_df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      daily_load = if (any(!is.na(.data$load))) {
        sum(.data$load, na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    )

  # Create complete time series starting from the first real activity date.
  # Days between first_activity_date and last recorded date with no activity
  # are genuine rest days (load = 0). Days before first_activity_date are not
  # included, so rollmean does not average over imputed zeros.
  all_dates_sequence <- seq(fetch_start_date, analysis_end_date, by = "day")
  training_dates <- daily_load_summary$date
  daily_load_complete <- dplyr::tibble(date = all_dates_sequence) %>%
    dplyr::left_join(daily_load_summary, by = "date") %>%
    dplyr::mutate(
      is_rest_day = !(.data$date %in% training_dates),
      daily_load = as.numeric(.data$daily_load)
    ) %>%
    dplyr::arrange(.data$date)

  if (missing_load == "zero") {
    # Legacy: rest day and missing-data training day both become 0.
    daily_load_complete$daily_load <- dplyr::coalesce(
      daily_load_complete$daily_load, 0
    )
  } else {
    # Only genuine rest days are imputed to 0. Missing-data training days
    # stay NA; downstream EWMA / RA logic treats them as gaps rather than
    # as zero-load sessions.
    daily_load_complete$daily_load <- ifelse(
      daily_load_complete$is_rest_day,
      0,
      daily_load_complete$daily_load
    )
  }
  daily_load_complete$is_rest_day <- NULL

  # --- Calculate ACWR based on method ---
  if (method == "ra") {
    acwr_data <- calculate_acwr_ra_internal(
      daily_load_complete, acute_period,
      chronic_period, smoothing_period,
      analysis_start_date, analysis_end_date
    )
  } else {
    acwr_data <- calculate_acwr_ewma_internal(
      daily_load_complete, half_life_acute,
      half_life_chronic, smoothing_period,
      analysis_start_date, analysis_end_date,
      ci, B, block_len, conf_level
    )
  }

  attr(acwr_data, "params") <- list(
    activity_type = activity_type,
    load_metric = load_metric,
    method = method,
    acute_period = acute_period,
    chronic_period = chronic_period,
    half_life_acute = half_life_acute,
    half_life_chronic = half_life_chronic,
    smoothing_period = smoothing_period,
    missing_load = missing_load,
    ci = ci,
    B = if (ci) B else NULL,
    block_len = if (ci) block_len else NULL,
    conf_level = if (ci) conf_level else NULL,
    start_date = analysis_start_date,
    end_date = analysis_end_date
  )
  class(acwr_data) <- c("athlytics_acwr", class(acwr_data))

  athlytics_message("Calculation complete.")
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
        .data$acute_load / .data$chronic_load, NA
      ),
      acwr_smooth = zoo::rollmean(.data$acwr, k = smoothing_period, align = "right", fill = NA)
    ) %>%
    dplyr::filter(.data$date >= start_date & .data$date <= end_date) %>%
    dplyr::select("date",
      atl = "acute_load", ctl = "chronic_load",
      "acwr", "acwr_smooth"
    )

  return(acwr_data)
}


#' Internal: Calculate ACWR using EWMA
#' @keywords internal
#' @noRd
calculate_acwr_ewma_internal <- function(daily_load_complete, half_life_acute, half_life_chronic,
                                         smoothing_period, start_date, end_date,
                                         ci, B, block_len, conf_level) {
  # Calculate alpha from half-life: alpha = 1 - exp(-ln(2) / half_life)
  alpha_acute <- 1 - exp(-log(2) / half_life_acute)
  alpha_chronic <- 1 - exp(-log(2) / half_life_chronic)

  # Calculate EWMA loads
  loads <- daily_load_complete$daily_load
  ewma <- calculate_ewma_loads(loads, alpha_acute, alpha_chronic, smoothing_period)

  # Build base result
  acwr_data <- data.frame(
    date = daily_load_complete$date,
    atl = ewma$acute_load,
    ctl = ewma$chronic_load,
    acwr = ewma$acwr,
    acwr_smooth = ewma$acwr_smooth
  ) %>%
    dplyr::filter(.data$date >= start_date & .data$date <= end_date)

  # --- Bootstrap Confidence Bands ---
  if (ci) {
    athlytics_message(sprintf(
      "Calculating %d%% confidence bands using %d bootstrap iterations...",
      conf_level * 100, B
    ))

    ci_bounds <- bootstrap_acwr_ci(
      loads, alpha_acute, alpha_chronic,
      smoothing_period, B, block_len, conf_level
    )

    ci_data <- data.frame(
      date = daily_load_complete$date,
      acwr_lower = ci_bounds$lower,
      acwr_upper = ci_bounds$upper
    )
    acwr_data <- dplyr::left_join(acwr_data, ci_data, by = "date")
  }

  return(acwr_data)
}


#' Internal: Calculate EWMA Load Series
#' @keywords internal
#' @noRd
calculate_ewma_loads <- function(loads, alpha_acute, alpha_chronic,
                                 smoothing_period) {
  n <- length(loads)
  acute_load <- rep(NA_real_, n)
  chronic_load <- rep(NA_real_, n)
  last_acute <- NA_real_
  last_chronic <- NA_real_

  for (i in seq_len(n)) {
    current_load <- loads[i]
    if (is.na(current_load)) {
      next
    }

    if (is.na(last_acute) || is.na(last_chronic)) {
      last_acute <- current_load
      last_chronic <- current_load
    } else {
      last_acute <- alpha_acute * current_load + (1 - alpha_acute) * last_acute
      last_chronic <- alpha_chronic * current_load + (1 - alpha_chronic) * last_chronic
    }

    acute_load[i] <- last_acute
    chronic_load[i] <- last_chronic
  }

  acwr <- ifelse(!is.na(chronic_load) & chronic_load > 0.01,
    acute_load / chronic_load,
    NA_real_
  )

  acwr_smooth <- zoo::rollmean(acwr, k = smoothing_period, align = "right", fill = NA)

  list(
    acute_load = acute_load,
    chronic_load = chronic_load,
    acwr = acwr,
    acwr_smooth = acwr_smooth
  )
}


#' Internal: Resample Loads with Overlapping Moving Blocks
#' @keywords internal
#' @noRd
moving_block_bootstrap_loads <- function(loads, block_len, starts = NULL) {
  n <- length(loads)
  if (n == 0) {
    return(loads)
  }

  block_len <- min(max(1L, as.integer(block_len)), n)
  n_blocks <- ceiling(n / block_len)
  max_start <- n - block_len + 1L

  if (is.null(starts)) {
    starts <- sample.int(max_start, n_blocks, replace = TRUE)
  } else {
    starts <- as.integer(starts)
    if (length(starts) < n_blocks ||
      any(is.na(starts)) ||
      any(starts < 1L) ||
      any(starts > max_start)) {
      stop("`starts` must contain valid moving-block start positions.")
    }
    starts <- starts[seq_len(n_blocks)]
  }

  boot_loads <- unlist(lapply(starts, function(start_pos) {
    loads[seq.int(start_pos, length.out = block_len)]
  }), use.names = FALSE)

  boot_loads[seq_len(n)]
}


#' Internal: Bootstrap Confidence Intervals for EWMA ACWR
#' @keywords internal
#' @noRd
bootstrap_acwr_ci <- function(loads, alpha_acute, alpha_chronic,
                              smoothing_period, B, block_len, conf_level) {
  n <- length(loads)

  # Store bootstrap ACWR values
  boot_acwr_matrix <- matrix(NA, nrow = n, ncol = B)

  for (b in 1:B) {
    # Moving-block bootstrap: sample overlapping windows with replacement.
    boot_loads <- moving_block_bootstrap_loads(loads, block_len)

    # Calculate EWMA for this bootstrap sample. Missing-load days are
    # reported as NA at that position, but the recursive state resumes on
    # the next finite load rather than poisoning every later day.
    boot_acwr_matrix[, b] <- calculate_ewma_loads(
      boot_loads, alpha_acute, alpha_chronic, smoothing_period
    )$acwr_smooth
  }

  # Calculate percentiles
  alpha_level <- 1 - conf_level
  lower_quantile <- alpha_level / 2
  upper_quantile <- 1 - alpha_level / 2

  safe_quantile <- function(x, probs) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(NA_real_)
    }
    unname(stats::quantile(x, probs = probs, na.rm = TRUE))
  }

  acwr_lower <- apply(boot_acwr_matrix, 1, safe_quantile, probs = lower_quantile)
  acwr_upper <- apply(boot_acwr_matrix, 1, safe_quantile, probs = upper_quantile)

  list(lower = acwr_lower, upper = acwr_upper)
}
