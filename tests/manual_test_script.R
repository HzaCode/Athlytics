# tests/manual_test_script.R
#
# Purpose: Manually test the core functionalities of the Athlytics package
#          using a real Strava API token (stoken).
#
# Instructions:
# 1. Make sure your working directory is the root of the Athlytics package.
# 2. Uncomment the Strava Authentication section and fill in your
#    app_name, client_id, and client_secret.
# 3. Run the script section by section in an interactive R session (like RStudio)
#    or run the whole script. Note that plots might only display correctly
#    in an interactive session.
# 4. Modify parameters like activity_type, load_metric, user_ftp, etc.,
#    as needed for your specific tests.

# --- Setup ---
message("Loading Athlytics package from source...")
# Ensure devtools is installed: install.packages("devtools")
# Ensure rStrava is installed: install.packages("rStrava")
# Ensure other dependencies are installed (dplyr, lubridate, zoo, purrr, ggplot2, etc.)
tryCatch({
  # Use load_all to load the development version of the package
  devtools::load_all()
  library(rStrava) # Need this for authentication and potentially helper functions if not re-exported
  library(dplyr)   # Useful for data manipulation below
  library(lubridate) # Useful for date handling
}, error = function(e) {
  stop("Error loading packages. Make sure devtools, rStrava, and package dependencies are installed. Error: ", e$message)
})

# --- Strava Authentication ---
# !! Uncomment and fill in your details below !!
# !! Run this section interactively to complete browser authentication !!

# message("Performing Strava Authentication...")
# app_name <- "YourAppName" # Choose a name for your app
# client_id <- 123456      # Replace with your actual client ID
# client_secret <- "YOUR_CLIENT_SECRET" # Replace with your actual client secret

# stoken <- tryCatch({
#   rStrava::strava_oauth(app_name = app_name,
#                         app_client_id = client_id,
#                         app_secret = client_secret,
#                         cache = TRUE) # Cache the token for future use
# }, error = function(e) {
#   stop("Strava authentication failed: ", e$message)
# })

# message("Authentication successful. 'stoken' object created.")

# --- Check if stoken exists ---
if (!exists("stoken") || !inherits(stoken, "Token2.0")) {
  stop("Strava token 'stoken' not found or invalid. Please run the authentication section first.")
}

# --- Pre-fetch initial activity list ---
message("\\n--- Fetching initial batch of activities (for reference) ---")
initial_activity_df <- NULL
tryCatch({
  # Fetch recent activities using rStrava defaults
  initial_activity_list <- rStrava::get_activity_list(stoken)
  if (length(initial_activity_list) > 0) {
    initial_activity_df <- rStrava::compile_activities(initial_activity_list)
    message(sprintf("Fetched and compiled %d initial activities into 'initial_activity_df'.", nrow(initial_activity_df)))
    # You can inspect initial_activity_df here if running interactively
    # print(head(initial_activity_df))
  } else {
    message("No recent activities found.")
  }
}, error = function(e) {
  warning("Could not pre-fetch initial activity list: ", e$message)
})
# NOTE: Subsequent function calls below (calculate_exposure, calculate_decoupling, calculate_pbs)
# currently DO NOT use this 'initial_activity_df'. They will perform their own API calls
# as needed based on their internal logic, unless the functions themselves are modified
# to accept pre-fetched data.

# --- 1. Exposure Calculation and Plotting ---
message("\\n--- Testing Exposure Calculation (Duration Metric) ---")
tryCatch({
  exposure_data_dur <- calculate_exposure(
    stoken = stoken,
    activity_type = c("Run", "Ride"), # Test with multiple types
    load_metric = "duration_mins",
    acute_period = 7,
    chronic_period = 42,
    end_date = Sys.Date() # Use today as end date
  )
  print("Exposure data (duration):")
  print(head(exposure_data_dur))
  print(tail(exposure_data_dur))

  message("\\n--- Testing Exposure Plot (Duration Metric) ---")
  # Note: Plot might not render if script is run non-interactively
  exposure_plot_dur <- plot_exposure(exposure_df = exposure_data_dur)
  print(exposure_plot_dur) # Try printing the plot object
  message("Exposure plot command executed (may require interactive session to view).")

}, error = function(e) {
  warning("Error during Exposure (Duration) test: ", e$message)
})

# Example for TSS (uncomment and provide FTP if needed)
message("\\n--- Testing Exposure Calculation (TSS Metric - requires FTP) ---")
tryCatch({
  exposure_data_tss <- calculate_exposure(
    stoken = stoken,
    activity_type = c("Ride", "VirtualRide"), # Activities with power data
    load_metric = "tss",
    user_ftp = 280, # <-- Set based on user input
    acute_period = 7,
    chronic_period = 28
  )
  print("Exposure data (TSS):")
  print(head(exposure_data_tss))
  print(tail(exposure_data_tss))
  exposure_plot_tss <- plot_exposure(exposure_df = exposure_data_tss)
  print(exposure_plot_tss)
  message("Exposure plot command executed (may require interactive session to view).")
}, error = function(e) {
  warning("Error during Exposure (TSS) test: ", e$message)
})

# Example for HRSS (added based on user input)
message("\\n--- Testing Exposure Calculation (HRSS Metric - requires Max/Resting HR) ---")
tryCatch({
  exposure_data_hrss <- calculate_exposure(
    stoken = stoken,
    activity_type = c("Run", "VirtualRun", "Ride"), # Activities with HR data
    load_metric = "hrss",
    user_max_hr = 220,     # <-- Set based on user input
    user_resting_hr = 60, # <-- Set based on user input
    acute_period = 7,
    chronic_period = 42
  )
  print("Exposure data (HRSS):")
  print(head(exposure_data_hrss))
  print(tail(exposure_data_hrss))
  exposure_plot_hrss <- plot_exposure(exposure_df = exposure_data_hrss)
  print(exposure_plot_hrss)
  message("Exposure plot command executed (may require interactive session to view).")
}, error = function(e) {
  warning("Error during Exposure (HRSS) test: ", e$message)
})


# --- 2. Decoupling Calculation and Plotting ---
# Let the function fetch recent activities and calculate decoupling for eligible ones.
message("\\n--- Testing Decoupling Calculation (Pace/HR for recent Runs) ---")
tryCatch({
  # Let calculate_decoupling handle fetching recent activities of type "Run"
  decoupling_data_runs <- calculate_decoupling(
    stoken = stoken,
    activity_type = "Run",       # Focus on Runs
    decouple_metric = "Pace_HR",
    max_activities = 10        # Limit number of activities to fetch/process for the test
    # min_duration_mins can be left as default or specified
  )
  print("Decoupling data (Recent Runs):")
  print(decoupling_data_runs) # Expect multiple rows if eligible runs are found

  message("\\n--- Testing Decoupling Plot (Pace/HR) ---")
  # plot_decoupling currently expects streams for a *single* activity.
  # We would need to adapt plot_decoupling or choose one result to plot.
  # For now, just confirm the calculation ran.
  message("Decoupling calculation successful for recent runs. Plotting function might need adaptation for multiple activities or require single stream input.")

}, error = function(e) {
  warning("Error during Decoupling (Recent Runs Pace/HR) test: ", e$message)
})

# --- 3. PB Calculation and Plotting ---
message("\\n--- Testing PB Calculation (Run distances) ---")
tryCatch({
  pb_distances_run <- c(1000, 1609, 5000, 10000, 21097, 42195) # Meters
  pb_data_run <- calculate_pbs(
    stoken = stoken,
    activity_type = "Run",
    distance_meters = pb_distances_run, # Corrected parameter name
    max_activities = 50  # Limit activities for testing to avoid rate limits
  )
  print("PB data (Run):")
  print(pb_data_run)

  message("\\n--- Testing PB Plot (Run distances) ---")
  # Note: Plot might not render if script is run non-interactively
  pb_plot_run <- plot_pbs(pb_data_run)
  print(pb_plot_run)
  message("PB plot command executed (may require interactive session to view).")

}, error = function(e) {
  warning("Error during PB (Run) test: ", e$message)
})

# Example for Ride distances (if applicable)
# message("\\n--- Testing PB Calculation (Ride distances) ---")
# tryCatch({
#   pb_distances_ride <- c(10000, 20000, 40000, 80000, 160934) # e.g., 10k, 20k, 40k, 50 miles, 100 miles
#   pb_data_ride <- calculate_pbs(
#     stoken = stoken,
#     activity_type = "Ride",
#     pb_distances_m = pb_distances_ride
#   )
#   print("PB data (Ride):")
#   print(pb_data_ride)
#   pb_plot_ride <- plot_pbs(pb_data_ride)
#   print(pb_plot_ride)
# }, error = function(e) {
#  warning("Error during PB (Ride) test: ", e$message)
# })


message("\\n--- Manual Test Script Finished ---") 