# R/utils.R

# Internal helper function for English month-year labels
english_month_year <- function(dates) {
  months_en <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
  paste(months_en[lubridate::month(dates)], lubridate::year(dates))
}

athlytics_is_verbose <- function() {
  isTRUE(getOption("Athlytics.verbose", FALSE))
}

athlytics_message <- function(..., .verbose = athlytics_is_verbose()) {
  if (isTRUE(.verbose)) {
    message(...)
  }
}

# Add other internal utility functions here in the future if needed
