library(dplyr)
# tests/testthat/helper-mockdata.R

mock_pbs_df <- data.frame(
  activity_id = c("1", "2", "3", "4"),
  activity_date = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-15 10:00:00", "2023-02-01 10:00:00", "2023-02-15 10:00:00")),
  distance = rep(c(5000, 10000), each = 2),
  time_seconds = c(1500, 1450, 3200, 3100),
  cumulative_pb_seconds = c(1500, 1450, 3200, 3100),
  is_pb = c(TRUE, TRUE, TRUE, TRUE),
  distance_label = factor(rep(c("5k", "10k"), each = 2), levels = c("5k", "10k")),
  stringsAsFactors = FALSE
)

mock_acwr_df <- data.frame(
  date = seq(lubridate::ymd("2023-01-01"), lubridate::ymd("2023-02-10"), by = "day"),
  atl = round(runif(41, 30, 70) + sin(seq(0, 4 * pi, length.out = 41)) * 10, 1),
  ctl = round(runif(41, 40, 60) + sin(seq(0, 2 * pi, length.out = 41)) * 5, 1)
) %>%
  dplyr::mutate(
    ctl_safe = ifelse(ctl <= 0, 1, ctl),
    acwr = round(atl / ctl_safe, 2),
    acwr_smooth = acwr
  ) %>%
  dplyr::select(date, atl, ctl, acwr, acwr_smooth)

latlng_list <- lapply(1:3601, function(i) c(runif(1, 40, 41), runif(1, -75, -74)))

mock_activity_streams <- data.frame(
  time = seq(0, 3600, by = 1),
  latlng = I(latlng_list),
  distance = seq(0, 10000, length.out = 3601),
  altitude = rnorm(3601, 100, 10),
  velocity_smooth = rnorm(3601, 3, 0.1), # Reduced variation for steady state
  heartrate = round(rnorm(3601, 150, 5)), # More stable HR
  cadence = round(runif(3601, 85, 95)),
  watts = round(rnorm(3601, 200, 10)), # More stable power
  grade_smooth = rnorm(3601, 0, 1),
  moving = sample(c(TRUE, FALSE), 3601, replace = TRUE, prob = c(0.95, 0.05)),
  temp = rnorm(3601, 20, 3)
)

mock_ef_df <- data.frame(
  activity_id = c("1", "2", "3", "4", "5"),
  date = lubridate::ymd(c("2023-01-01", "2023-01-15", "2023-02-01", "2023-02-15", "2023-03-01")),
  activity_type = rep("Run", 5),
  ef_value = round(rnorm(5, 1.5, 0.1), 2),
  ef_metric = rep("pace_hr", 5),
  stringsAsFactors = FALSE
)

mock_exposure_df <- data.frame(
  date = seq(lubridate::ymd("2023-01-01"), lubridate::ymd("2023-02-10"), by = "day"),
  atl = round(runif(41, 30, 70) + sin(seq(0, 4 * pi, length.out = 41)) * 10, 1),
  ctl = round(runif(41, 40, 60) + sin(seq(0, 2 * pi, length.out = 41)) * 5, 1)
) %>%
  dplyr::mutate(
    ctl_safe = ifelse(ctl <= 0, 1, ctl),
    acwr = round(atl / ctl_safe, 2)
  )

mock_activity_list_df <- data.frame(
  id = c(1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010),
  name = paste("Activity", 1:10),
  start_date_local = seq.POSIXt(as.POSIXct("2023-10-01 08:00:00", tz = "UTC"),
    by = "-2 days", length.out = 10
  ),
  type = rep(c("Run", "Ride"), length.out = 10),
  distance = c(5050, 20100, 10200, 30500, 8030, 15200, 12100, 40300, 6050, 25400),
  moving_time = c(1800, 3600, 3000, 5400, 2400, 2700, 3300, 7200, 1900, 4500),
  elapsed_time = c(1850, 3700, 3060, 5500, 2450, 2780, 3380, 7350, 1950, 4600),
  total_elevation_gain = c(50, 150, 100, 250, 80, 120, 110, 300, 60, 200),
  average_heartrate = c(150, 140, 155, 145, 152, 142, 158, 148, 151, 141),
  max_heartrate = c(170, 160, 175, 165, 172, 162, 178, 168, 171, 161),
  average_watts = c(NA, 200, NA, 220, NA, 190, NA, 230, NA, 210),
  stringsAsFactors = FALSE
)

mock_activity_list_df$start_date_local <- as.POSIXct(mock_activity_list_df$start_date_local, tz = "UTC")

# Adjust the date of the second "Run" activity (original index 3) to be within the test range
# The test expects two runs between 2023-10-01 and 2023-10-03
# Original dates: 2023-10-01 (Run), 2023-09-29 (Ride), 2023-09-27 (Run)
# Change 2023-09-27 to 2023-10-02 for the second Run
mock_activity_list_df$start_date_local[3] <- as.POSIXct("2023-10-02 09:00:00", tz = "UTC")

mock_activity_list_df$duration_mins <- mock_activity_list_df$elapsed_time / 60
mock_activity_list_df$distance_km <- mock_activity_list_df$distance / 1000

mock_activity_list_df_for_list <- mock_activity_list_df %>%
  dplyr::mutate(start_date_local = format(start_date_local, "%Y-%m-%d %H:%M:%S"))

mock_activity_list_list <- purrr::transpose(mock_activity_list_df_for_list)
