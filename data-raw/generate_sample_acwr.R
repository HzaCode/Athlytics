# Script to generate sample_acwr data

library(dplyr)
library(tibble)

set.seed(123)

# Generate 365 days of data
n_days <- 365
start_date <- as.Date("2023-01-01")
dates <- seq(start_date, by = "day", length.out = n_days)

# Simulate daily training load with weekly periodization
# Higher loads on weekdays, rest on some weekends
day_of_week <- as.POSIXlt(dates)$wday
base_load <- ifelse(day_of_week %in% c(0, 6),
  runif(n_days, 0, 30), # Weekend: lower/rest
  runif(n_days, 40, 90)
) # Weekday: training

# Add some seasonal variation (higher in spring/summer)
day_of_year <- as.numeric(format(dates, "%j"))
seasonal_factor <- 1 + 0.2 * sin(2 * pi * (day_of_year - 80) / 365)
daily_load <- base_load * seasonal_factor

# Add some random rest days (illness, travel, etc.)
rest_days <- sample(1:n_days, size = 30)
daily_load[rest_days] <- 0

# Calculate rolling averages for ATL and CTL
acute_window <- 7
chronic_window <- 28

atl <- zoo::rollmean(daily_load, k = acute_window, fill = NA, align = "right")
ctl <- zoo::rollmean(daily_load, k = chronic_window, fill = NA, align = "right")

# Calculate ACWR (with protection against division by zero)
acwr <- ifelse(ctl > 0, atl / ctl, NA)

# Apply smoothing to ACWR
acwr_smooth <- zoo::rollmean(acwr, k = 7, fill = NA, align = "right")

# Create the dataset
sample_acwr <- tibble(
  date = dates,
  atl = round(atl, 2),
  ctl = round(ctl, 2),
  acwr = round(acwr, 3),
  acwr_smooth = round(acwr_smooth, 3)
)

# Remove rows with NA values (first few weeks)
sample_acwr <- sample_acwr[complete.cases(sample_acwr), ]

# Save to data directory
save(sample_acwr, file = "data/sample_acwr.rda", compress = "xz")

message("sample_acwr.rda generated with ", nrow(sample_acwr), " rows.")
