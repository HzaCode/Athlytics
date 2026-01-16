# Script to regenerate sample_pbs data
library(dplyr)
library(lubridate)
library(Athlytics) # Assumes current version is loaded

set.seed(42)

# Define time range
start_date <- as.Date("2023-01-01")
dates <- seq(start_date, by = "1 month", length.out = 12)

# Function to generate improvement trend
generate_trend <- function(base_time, improvement_rate, noise_sd, n) {
  time_seq <- base_time * (1 - seq(0, improvement_rate, length.out = n))
  time_seq + rnorm(n, 0, noise_sd)
}

# 1k Data (Improving from ~5:00 to ~4:15)
times_1k <- generate_trend(300, 0.15, 5, 12)
df_1k <- data.frame(
  activity_date = as.POSIXct(dates),
  distance = 1000,
  time_seconds = times_1k,
  distance_label = "1k"
)

# 5k Data (Improving from ~25:00 to ~21:15) - fewer points
dates_5k <- seq(start_date, by = "2 months", length.out = 6)
times_5k <- generate_trend(1500, 0.15, 15, 6)
df_5k <- data.frame(
  activity_date = as.POSIXct(dates_5k),
  distance = 5000,
  time_seconds = times_5k,
  distance_label = "5k"
)

# 10k Data (Improving from ~52:00 to ~45:00) - fewer points
dates_10k <- seq(start_date, by = "3 months", length.out = 4)
times_10k <- generate_trend(3120, 0.13, 30, 4)
df_10k <- data.frame(
  activity_date = as.POSIXct(dates_10k),
  distance = 10000,
  time_seconds = times_10k,
  distance_label = "10k"
)

# Combine all data
all_data <- rbind(df_1k, df_5k, df_10k) %>%
  arrange(activity_date) %>%
  mutate(
    activity_id = paste0("activity_", row_number()),
    elapsed_time = time_seconds,
    moving_time = time_seconds,
    time_period = seconds_to_period(time_seconds)
  )

# Calculate cumulative PBs and is_pb flag
sample_pbs <- all_data %>%
  group_by(distance) %>%
  arrange(activity_date) %>%
  mutate(
    cumulative_pb_seconds = cummin(time_seconds),
    is_pb = time_seconds == cumulative_pb_seconds,
    # Ensure distance_label is a factor with correct levels
    distance_label = factor(distance_label, levels = c("1k", "5k", "10k"))
  ) %>%
  ungroup() %>%
  # Select and order columns to match package documentation
  select(
    activity_id, activity_date, distance, elapsed_time, moving_time,
    time_seconds, cumulative_pb_seconds, is_pb, distance_label, time_period
  )

# Convert to tibble for consistency
sample_pbs <- as_tibble(sample_pbs)

# Save to data directory
save(sample_pbs, file = "data/sample_pbs.rda", compress = "xz")

message("sample_pbs.rda regenerated with ", nrow(sample_pbs), " rows.")
print(table(sample_pbs$distance_label))
print(table(sample_pbs$is_pb))
