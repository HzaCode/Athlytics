# tests/test_direct_api_streams.R
# Description: Script to test direct Strava API call for activity streams, bypassing rStrava functions.

# --- Libraries ---
# install.packages(c("rStrava", "httr", "jsonlite", "dplyr")) # Uncomment if needed
library(rStrava)
library(httr)
library(jsonlite)
library(dplyr)

# --- Strava Authentication ---
# IMPORTANT: Ensure these match your Strava API Application settings
# DO NOT SHARE YOUR SECRET PUBLICLY
app_name <- "api_testing"      # Or your specific app name
app_client_id <- "118977"   # Replace with your Client ID
app_secret <- "YOUR_SECRET_HERE" # <--- !!! REPLACE WITH YOUR ACTUAL SECRET !!! 

message("\n--- Attempting Strava Authentication ---")
stoken <- tryCatch({
    rStrava::strava_oauth(app_name,
                          app_client_id = app_client_id,
                          app_secret = app_secret,
                          app_scope = "activity:read_all", 
                          cache = TRUE) # Use cache if available
}, error = function(e) {
    message("Error during authentication: ", e$message)
    NULL
})

if (is.null(stoken) || !inherits(stoken, "Token2.0")) {
  stop("Strava authentication failed or token is invalid.")
} else {
  message("Authentication successful or token loaded from cache.")
  
  # --- NEW: Detailed check of stoken object structure ---
  message("--- Checking stoken object structure ---")
  print(str(stoken)) # Print the structure of the token object
  
  # Explicitly check for credentials and access_token
  if (!is.list(stoken$credentials) || is.null(stoken$credentials$access_token) || nchar(stoken$credentials$access_token) == 0) {
    stop("ERROR: stoken object exists but does not contain a valid $credentials$access_token.")
  } else {
    message("SUCCESS: Found valid access token within stoken object.")
    # Optionally print the first few characters of the token for confirmation (DO NOT PRINT THE WHOLE TOKEN)
    # message(paste("Access Token starts with:", substr(stoken$credentials$access_token, 1, 6)))
  }
  message("--- Finished checking stoken object ---")
}

# --- Test Parameters ---
# Choose an activity ID known to cause issues previously
test_activity_id <- "13742582603" # Replace if you want to test a different ID

# Define the stream types you want to request
# For decoupling, we typically need time, heartrate, and distance/velocity_smooth or watts
stream_keys_to_request <- c("time", "heartrate", "distance", "velocity_smooth", "watts")
keys_param <- paste(stream_keys_to_request, collapse = ",")

message(sprintf("\n--- Testing Direct API Call for Activity ID: %s ---", test_activity_id))
message(sprintf("Requesting stream keys: %s", keys_param))

# --- Direct API Call Logic ---
# Now we are more confident that stoken$credentials$access_token exists
api_streams_result <- tryCatch({
    
    # 1. Extract Access Token
    access_token <- stoken$credentials$access_token
    if (is.null(access_token) || nchar(access_token) == 0) {
        stop("Could not extract access token from stoken object.")
    }
    message("Access token extracted.")

    # 2. Construct API URL
    api_url <- sprintf("https://www.strava.com/api/v3/activities/%s/streams?keys=%s&key_by_type=true",
                       test_activity_id,
                       keys_param)
    message(sprintf("Requesting URL: %s", api_url))

    # 3. Make the GET request
    response <- httr::GET(
        url = api_url,
        config = httr::add_headers(Authorization = paste("Bearer", access_token)),
        httr::timeout(30), # Add a timeout (e.g., 30 seconds)
        httr::verbose()    # Add verbose output for detailed connection info
    )
    message(sprintf("API Response Status: %d", httr::status_code(response))) 

    # 4. Check for HTTP errors
    httr::stop_for_status(response, task = paste("fetch streams for activity", test_activity_id))

    # 5. Parse JSON content
    message("Parsing JSON response...")
    parsed_content <- httr::content(response, "text", encoding = "UTF-8")
    streams_list <- jsonlite::fromJSON(parsed_content, simplifyVector = FALSE) # Use FALSE first

    # 6. Analyze the response
    available_streams <- names(streams_list)
    message(sprintf("Successfully parsed. Available stream types returned by API: %s", 
                    ifelse(length(available_streams) > 0, paste(available_streams, collapse = ", "), "NONE")))

    # Check for essential streams
    if (!("time" %in% available_streams) || is.null(streams_list$time$data) || length(streams_list$time$data) == 0) {
         message("CRITICAL ERROR: 'time' stream is missing, NULL, or empty in the API response!")
    } else {
         message(sprintf("'time' stream found with %d data points.", length(streams_list$time$data)))
    }
    
    if (!("heartrate" %in% available_streams) || is.null(streams_list$heartrate$data) || length(streams_list$heartrate$data) == 0) {
         message("WARNING: 'heartrate' stream is missing, NULL, or empty in the API response!")
    } else {
         message(sprintf("'heartrate' stream found with %d data points.", length(streams_list$heartrate$data)))
         # Optional: Check length consistency with time stream
         if (exists("streams_list$time$data") && length(streams_list$heartrate$data) != length(streams_list$time$data)){
            message(sprintf("WARNING: heartrate stream length (%d) does not match time stream length (%d).", 
                            length(streams_list$heartrate$data), length(streams_list$time$data)))
         }
    }
    
    # Check for distance/velocity/watts
    if (!any(c("distance", "velocity_smooth") %in% available_streams)) {
        message("INFO: Neither 'distance' nor 'velocity_smooth' stream found.")
    } else {
        if("distance" %in% available_streams) message("'distance' stream found.")
        if("velocity_smooth" %in% available_streams) message("'velocity_smooth' stream found.")
    }
    if (!("watts" %in% available_streams)) {
         message("INFO: 'watts' stream not found.")
    } else {
        message("'watts' stream found.")
    }
    
    # Return the parsed list for potential further inspection
    streams_list 

}, error = function(e) {
    message("\n--- ERROR during direct API call or processing ---")
    message(e$message)
    # Print the full error object if needed
    # print(e) 
    NULL 
})

message("\n--- Test Script Finished ---")

# You can inspect the 'api_streams_result' object in your R console if the call was successful
# print(str(api_streams_result)) 