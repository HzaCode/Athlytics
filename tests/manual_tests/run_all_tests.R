# --- Test Script for Athlytics Package Functions ---

# --- 0. Load Package ---
message("Loading Athlytics package using library(Athlytics)...")
tryCatch({
  library(Athlytics)
}, error = function(e) {
  stop("Error loading package with library(Athlytics). Is the package installed? Error: ", e$message)
})

# --- 1. Load Required Libraries ---
message("Loading required libraries (rStrava, httr, dplyr)...")
tryCatch({
  library(rStrava)
  library(httr) # Needed for Token object check
  library(dplyr) # Often used internally or helpful for checks
}, error = function(e) {
  stop("Error loading required libraries (rStrava, httr, dplyr). Are they installed? Error: ", e$message)
})

cat("Libraries loaded.\n")

# --- 2. Strava Authentication ---
# Attempts authentication. Stops script on failure.
message("Performing Strava Authentication...")
app_name <- 'MyAthlyticsApp'
# Use environment variables or secure methods in real CI/automated scenarios
client_id <- Sys.getenv("STRAVA_CLIENT_ID", "118977") # Default if env var not set
client_secret <- Sys.getenv("STRAVA_CLIENT_SECRET", "1bc6d6c3c26c114e7479ba561adadbf7faaa8057") # Default if env var not set

stoken <- tryCatch({
  cat("Attempting Strava authentication using rStrava::strava_oauth()...\n")
  cat("Using Client ID: ", client_id, "\n") # Log the ID being used
  cat("Check browser if prompted for authorization.\n")
  rStrava::strava_oauth(app_name = app_name,
                        app_client_id = client_id, # Use correct param name
                        app_secret = client_secret, # Use correct param name
                        app_scope = "activity:read_all",
                        cache = TRUE) # Cache the token for future use
}, error = function(e) {
  stop("Strava authentication failed: ", e$message) # Stop script on error
})

if (!is.null(stoken) && inherits(stoken, "Token2.0")) {
    message("Authentication successful or valid cached token loaded.")
} else {
    # This case might not be reachable if stop() always works on error
    stop("Authentication process did not return a valid token object.")
}

# --- 3. Define Test Parameters ---
message("Defining test parameters...")
# Use a reasonable number for testing, avoid hitting rate limits excessively
test_max_activities <- 5 # Reducing from 10 to 5 for faster testing
user_ftp <- 250
user_max_hr <- 190
user_resting_hr <- 60

message(sprintf("Using max_activities = %d for relevant tests.", test_max_activities))
message(sprintf("Using FTP = %d, Max HR = %d, Resting HR = %d where needed.", user_ftp, user_max_hr, user_resting_hr))


# --- 4. Run Tests ---
# Each block wrapped in tryCatch to allow continuation if one test fails
# -- Test: calculate_exposure (TSS) and plot_exposure --
message("\n--- Testing: calculate_exposure (TSS) & plot_exposure ---")
tryCatch({
  exposure_res_tss <- calculate_exposure(
    stoken = stoken, activity_type = "Run",
    load_metric = "tss", user_ftp = user_ftp,
    acute_period = 7, chronic_period = 28
  )
  message("calculate_exposure (TSS) finished. Result preview:")
  print(head(exposure_res_tss))
  message("Plotting exposure (TSS)...")
  p_exp_tss <- plot_exposure(exposure_df = exposure_res_tss)
  print(p_exp_tss) # Uncommented to display plot
  message("plot_exposure (TSS) command executed.")
}, error = function(e) {
  warning("FAILED: calculate_exposure (TSS) or plot_exposure failed: ", e$message)
})

Sys.sleep(5) # Add delay between test blocks

# -- Test: calculate_exposure (HRSS) and plot_exposure --
message("\n--- Testing: calculate_exposure (HRSS) & plot_exposure ---")
tryCatch({
  exposure_res_hrss <- calculate_exposure(
    stoken = stoken, activity_type = "Run",
    load_metric = "hrss", user_max_hr = user_max_hr,
    user_resting_hr = user_resting_hr,
    acute_period = 7, chronic_period = 28
  )
  message("calculate_exposure (HRSS) finished. Result preview:")
  print(head(exposure_res_hrss))
  message("Plotting exposure (HRSS)...")
  p_exp_hrss <- plot_exposure(exposure_df = exposure_res_hrss)
  print(p_exp_hrss) # Uncommented to display plot
  message("plot_exposure (HRSS) command executed.")
}, error = function(e) {
  warning("FAILED: calculate_exposure (HRSS) or plot_exposure failed: ", e$message)
})

Sys.sleep(5) # Add delay between test blocks

# -- Test: calculate_acwr and plot_acwr --
message("\n--- Testing: calculate_acwr (duration) & plot_acwr ---")
tryCatch({
  acwr_res <- calculate_acwr(
    stoken = stoken, activity_type = "Run",
    load_metric = "duration_mins", acute_period = 7,
    chronic_period = 28
  )
  message("calculate_acwr (duration) finished. Result preview:")
  print(head(acwr_res))
  message("Plotting ACWR (duration)...")
  p_acwr <- plot_acwr(acwr_data = acwr_res)
  print(p_acwr) # Uncommented to display plot
  message("plot_acwr (duration) command executed.")
}, error = function(e) {
  warning("FAILED: calculate_acwr (duration) or plot_acwr failed: ", e$message)
})

Sys.sleep(5) # Add delay between test blocks

# -- Test: calculate_ef and plot_ef --
message("\n--- Testing: calculate_ef (Pace_HR) & plot_ef ---")
tryCatch({
  ef_res <- calculate_ef(
    stoken = stoken, activity_type = "Run",
    ef_metric = "Pace_HR"
  )
  message("calculate_ef (Pace_HR) finished. Result preview:")
  print(head(ef_res))
  message("Plotting EF (Pace_HR)...")
  p_ef <- plot_ef(ef_df = ef_res)
  print(p_ef)
  message("plot_ef (Pace_HR) command executed.")
}, error = function(e) {
  warning("FAILED: calculate_ef (Pace_HR) or plot_ef failed: ", e$message)
})

Sys.sleep(5) # Add delay between test blocks

# -- Test: calculate_pbs and plot_pbs --
message("\n--- Testing: calculate_pbs (Run distances) & plot_pbs ---")
tryCatch({
  pb_distances <- c(1000, 5000, 10000) # Test a few common distances
  pbs_res <- calculate_pbs(
    stoken = stoken, distance_meters = pb_distances,
    activity_type = "Run", max_activities = test_max_activities
  )
  message("calculate_pbs (Run) finished. Results:")
  print(pbs_res)
  message("Plotting PBs (Run)...")
  p_pbs <- plot_pbs(pb_data = pbs_res)
  print(p_pbs) # Uncommented to display plot
  message("plot_pbs (Run) command executed.")
}, error = function(e) {
  warning("FAILED: calculate_pbs (Run) or plot_pbs failed: ", e$message)
})

Sys.sleep(5) # Add delay between test blocks

# -- Test: calculate_decoupling and plot_decoupling --
message("\n--- Testing: calculate_decoupling (Pace_HR) & plot_decoupling ---")
tryCatch({
  decoupling_res <- calculate_decoupling(
    stoken = stoken, activity_type = "Run",
    decouple_metric = "Pace_HR", max_activities = test_max_activities
  )
  message("calculate_decoupling (Pace_HR) finished. Results:")
  print(decoupling_res)
  message("Plotting Decoupling (Pace_HR)... Note: Plotting might expect single activity data.")
  # The plot function might need adaptation or specific input handling
  # p_decoupling <- plot_decoupling(decoupling_data = decoupling_res)
  # print(p_decoupling)
  message("plot_decoupling (Pace_HR) command skipped/commented out in script.") # Plotting still commented
}, error = function(e) {
  warning("FAILED: calculate_decoupling (Pace_HR) or plot_decoupling failed: ", e$message)
})

# --- 5. Script Finish ---
message("\n--- Testing Script Finished ---")
message("Review output above for PASS/FAILED status of individual tests.")
message("NOTE: Warnings indicate failures in specific test blocks.")

 