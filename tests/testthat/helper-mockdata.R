# tests/testthat/helper-mockdata.R

# 模拟 calculate_pbs 处理后的结果
# 这是一个简化的结构，实际列可能更多
mock_pbs_df <- data.frame(
  activity_id = c("1", "2", "3", "4"),
  activity_date = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-15 10:00:00", "2023-02-01 10:00:00", "2023-02-15 10:00:00")),
  distance = rep(c(5000, 10000), each = 2),
  time_seconds = c(1500, 1450, 3200, 3100), # 25:00, 24:10, 53:20, 51:40
  cumulative_pb_seconds = c(1500, 1450, 3200, 3100),
  is_pb = c(TRUE, TRUE, TRUE, TRUE), # 简化：假设每次都是 PB
  distance_label = factor(rep(c("5k", "10k"), each = 2), levels = c("5k", "10k")),
  stringsAsFactors = FALSE
)

# 可以添加更复杂的模拟数据，例如模拟 get_activity_list 或 get_activity 的原始返回
# mock_activities_list <- ...
# mock_detailed_activity <- ... 

# 模拟 calculate_acwr 处理后的结果
# 包含日期, atl, ctl, acwr, acwr_smooth
mock_acwr_df <- data.frame(
    date = seq(lubridate::ymd("2023-01-01"), lubridate::ymd("2023-02-10"), by="day"),
    # 模拟一些负荷值
    atl = round(runif(41, 30, 70) + sin(seq(0, 4*pi, length.out=41))*10, 1),
    ctl = round(runif(41, 40, 60) + sin(seq(0, 2*pi, length.out=41))*5, 1)
) %>%
  dplyr::mutate(
    # 确保 ctl 不为 0 以避免除零错误
    ctl_safe = ifelse(ctl <= 0, 1, ctl),
    acwr = round(atl / ctl_safe, 2),
    # 模拟平滑后的 ACWR (简单起见，这里直接用 acwr)
    acwr_smooth = acwr 
  ) %>%
  # 选择最终需要的列，包括 acwr_smooth
  dplyr::select(date, atl, ctl, acwr, acwr_smooth) 

# 模拟 get_streams 返回的数据结构 (单个活动)
# 实际列可能更多，类型也可能不同
# Create latlng as a list of vectors
latlng_list <- lapply(1:3601, function(i) c(runif(1, 40, 41), runif(1, -75, -74)))

mock_activity_streams <- data.frame(
  time = seq(0, 3600, by = 1), # 1 hour of data, 1 sample per second
  latlng = I(latlng_list), # Use the pre-generated list column
  distance = seq(0, 10000, length.out = 3601), # 10k distance
  altitude = rnorm(3601, 100, 10),
  velocity_smooth = rnorm(3601, 3, 0.5), # m/s
  heartrate = round(runif(3601, 130, 170) + sin(seq(0, 4 * pi, length.out = 3601)) * 10),
  cadence = round(runif(3601, 85, 95)),
  watts = round(runif(3601, 150, 250) + sin(seq(0, 2 * pi, length.out = 3601)) * 20),
  grade_smooth = rnorm(3601, 0, 1),
  moving = sample(c(TRUE, FALSE), 3601, replace = TRUE, prob = c(0.95, 0.05)),
  temp = rnorm(3601, 20, 3)
)

# 模拟 calculate_ef 处理后的结果
mock_ef_df <- data.frame(
  activity_id = c("1", "2", "3", "4", "5"),
  date = lubridate::ymd(c("2023-01-01", "2023-01-15", "2023-02-01", "2023-02-15", "2023-03-01")),
  activity_type = rep("Run", 5),
  ef_value = round(rnorm(5, 1.5, 0.1), 2), # 模拟 EF 值
  ef_metric = rep("Pace_HR", 5),
  stringsAsFactors = FALSE
)

# 模拟 calculate_exposure 处理后的结果
mock_exposure_df <- data.frame(
  date = seq(lubridate::ymd("2023-01-01"), lubridate::ymd("2023-02-10"), by="day"),
  atl = round(runif(41, 30, 70) + sin(seq(0, 4*pi, length.out=41))*10, 1),
  ctl = round(runif(41, 40, 60) + sin(seq(0, 2*pi, length.out=41))*5, 1)
) %>%
  dplyr::mutate(
    ctl_safe = ifelse(ctl <= 0, 1, ctl),
    acwr = round(atl / ctl_safe, 2) # plot_exposure uses this internally
  ) 

# Mock data simulating the output of rStrava::get_activity_list()
# Contains essential columns needed for Athlytics calculations.
mock_activity_list_df <- data.frame(
  id = c(1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010),
  name = paste("Activity", 1:10),
  start_date_local = seq.POSIXt(as.POSIXct("2023-10-01 08:00:00", tz = "UTC"), 
                              by = "-2 days", length.out = 10),
  type = rep(c("Run", "Ride"), length.out = 10),
  distance = c(5050, 20100, 10200, 30500, 8030, 15200, 12100, 40300, 6050, 25400), # meters
  moving_time = c(1800, 3600, 3000, 5400, 2400, 2700, 3300, 7200, 1900, 4500), # seconds
  elapsed_time = c(1850, 3700, 3060, 5500, 2450, 2780, 3380, 7350, 1950, 4600), # seconds
  total_elevation_gain = c(50, 150, 100, 250, 80, 120, 110, 300, 60, 200), # meters
  average_heartrate = c(150, 140, 155, 145, 152, 142, 158, 148, 151, 141),
  max_heartrate = c(170, 160, 175, 165, 172, 162, 178, 168, 171, 161),
  average_watts = c(NA, 200, NA, 220, NA, 190, NA, 230, NA, 210), # Watts (NA for Runs without power)
  stringsAsFactors = FALSE
)

# Convert start_date_local back to POSIXct just in case tz handling varies
# Use a common timezone like UTC for consistency in tests
mock_activity_list_df$start_date_local <- as.POSIXct(mock_activity_list_df$start_date_local, tz = "UTC")

# Optional: Add derived columns that might be useful or expected by functions
# Example: duration in minutes (using elapsed_time)
mock_activity_list_df$duration_mins <- mock_activity_list_df$elapsed_time / 60
mock_activity_list_df$distance_km <- mock_activity_list_df$distance / 1000


# --- Placeholder for future mock data ---
# e.g., mock_activity_streams_data <- ... 