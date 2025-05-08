Athlytics <img src="https://github.com/HzaCode/Athlytics/blob/main/image.png?raw=true" align="right" width="200"/>
=======================================================


[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN status: pending review](https://img.shields.io/badge/CRAN%20status-pending%20review-orange.svg)](https://CRAN.R-project.org/)
![](https://img.shields.io/badge/version-0.1.2-blue)
[![R-CMD-check](https://github.com/HzaCode/Athlytics/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/HzaCode/Athlytics/actions/workflows/R-CMD-check.yml)
[![codecov](https://codecov.io/gh/HzaCode/Athlytics/graph/badge.svg)](https://app.codecov.io/gh/HzaCode/Athlytics)
![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) 
[![Website](https://img.shields.io/badge/website-Athlytics-blue)](https://hezhiang.com/Athlytics/)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/HzaCode/Athlytics/pulls)
[![bioRxiv](https://img.shields.io/badge/bioRxiv-Preprint-F88A0B)](https://www.biorxiv.org/content/10.1101/2025.05.01.651597v2)
### Overview

This is the development repository for **Athlytics**, an R package designed to simplify the analysis of athletic performance and training load data sourced **directly from the Strava API**. The package provides functions for fetching your Strava data, calculating key metrics, and generating insightful visualizations for training monitoring.

### Installation

You can install the released version of Athlytics from CRAN (once available) with:

```r
install.packages("Athlytics")
```

Alternatively, the development version from this repository can be installed as follows:

```r
# install.packages('remotes')
remotes::install_github('HzaCode/Athlytics')
```


### Using Athlytics Functions

```r
library(Athlytics)
```

### Authentication with Strava (using `rStrava`)

`Athlytics` **requires** you to authenticate with Strava using the `rStrava` package. This generates a token that you then pass to `Athlytics` functions.

1. **Create a Strava API Application:**
    - Go to the Strava API documentation page ([https://developers.strava.com/docs/reference/](https://developers.strava.com/docs/reference/)) and find instructions for creating an application (you'll likely need to navigate to your settings page once logged in).
    - Create a new API application (e.g., "My Athlytics Analysis").
    - Set the "Authorization Callback Domain" to `localhost`.
    - Note your **Client ID** and **Client Secret**.
2. **Authenticate in R using `rStrava::strava_oauth()`:**
    - This function handles the OAuth2.0 flow, potentially opening a browser window for authorization.
    - It returns a `Token2.0` object needed by `Athlytics`.
    - **Use `cache = TRUE`** (default) to store the token securely (in `.httr-oauth`), avoiding re-authentication in later sessions.

Note: Running `strava_oauth()` may open a browser window for you to log in to Strava and authorize the application.

```r
library(Athlytics)

# Sys.setenv(STRAVA_CLIENT_ID = "YOUR_CLIENT_ID")
# Sys.setenv(STRAVA_CLIENT_SECRET = "YOUR_SECRET")

# --- Authentication Step ---
app_name <- 'MyAthlyticsApp' # Choose a name
client_id <- Sys.getenv("STRAVA_CLIENT_ID")
client_secret <- Sys.getenv("STRAVA_CLIENT_SECRET")

# Authenticate using rStrava and STORE the token object
# Make sure app_scope allows reading activities.
stoken <- rStrava::strava_oauth(app_name,
                                app_client_id = client_id, # Use app_client_id
                                app_secret = client_secret, # Use app_secret
                                app_scope = "activity:read_all", # Or specific scope needed
                                cache = TRUE) # IMPORTANT for reusing the token

```

### Optional Parameters for Specific Metrics

While most functions rely on data directly available from Strava (like duration, distance, heart rate), some advanced calculations or metrics **may require additional user-specific information**.

- `user_ftp` (Numeric): Your Functional Threshold Power (in Watts). Needed for calculating TSS (approximate) or power zones for cycling/running with power.
- `user_max_hr` (Numeric): Your maximum heart rate. Needed for calculating HRSS (approximate).
- `user_resting_hr` (Numeric): Your resting heart rate. Also needed for calculating HRSS (approximate).

*(Provide these parameters directly to the Athlytics function call if the chosen `load_metric` or analysis requires them. Check function documentation for details.)*

### Example Analysis Visualizations

These functions generate plots to analyze trends and performance, using data fetched and processed from Strava via the `stoken` you provide.

**Important Note on API Usage and Performance:** Some functions, particularly `calculate_pbs` (which needs to fetch detailed data for each activity) and `calculate_decoupling` (which now uses `httr` to directly fetch detailed activity streams), can be **slow** and may hit Strava API rate limits (leading to errors) if you analyze a large number of activities. Consider using the `max_activities` parameter in these functions or analyzing shorter date ranges if you encounter issues.

### 1. Load Exposure

This analysis provides an intuitive way to assess your current training load status and potential injury risk level by plotting acute vs. chronic load. You can clearly see where your load combination falls within defined risk zones (like the sweet spot, caution zone, or danger zone). **Note:** When using TSS or HRSS as the `load_metric`, the calculation is an *approximation* based on average power or average heart rate available from the Strava activity summary, not detailed stream data (which would be much slower to process).

**Calculating Data:**
To get the underlying data (date, daily_load, atl, ctl, acwr) as a data frame without plotting:
```r
# Ensure stoken is a valid token object from rStrava::strava_oauth()
# Ensure user_ftp = 280 is appropriate for the user
exposure_data <- calculate_exposure(
    stoken = stoken,
    activity_type = "Ride", # Example: Target Rides
    load_metric = "tss",    # Example: Use approximate TSS based on average power
    user_ftp = 280,         # REQUIRED for TSS
    acute_period = 7,
    chronic_period = 28
)
# print(tail(exposure_data)) # Uncomment to view
```

**Plotting:**
```r
# Example using approximated TSS based on avg power for Rides
# Ensure stoken is valid and user_ftp = 280 is correct
plot_exposure(
    stoken = stoken,
    activity_type = "Ride",        # Specify activity type(s)
    load_metric = "tss",           # Use approximate TSS
    user_ftp = 280,                # REQUIRED for TSS
    acute_period = 7,              # Duration (days) for acute load calculation
    chronic_period = 28            # Duration (days) for chronic load calculation
    # , end_date = Sys.Date()      # Optional
    # , risk_zones = TRUE          # Optional
)

# Example using approximate HRSS based on avg heart rate for Runs
# Ensure stoken is valid and HR parameters are correct
plot_exposure(
    stoken = stoken,
    activity_type = "Run",         # Specify activity type(s)
    load_metric = "hrss",          # Use approximate HRSS
    user_max_hr = 190,             # REQUIRED for HRSS
    user_resting_hr = 50,          # REQUIRED for HRSS
    acute_period = 7,              # Duration (days) for acute load calculation
    chronic_period = 42            # Example: Longer chronic window
    # , end_date = Sys.Date()      # Optional
    # , risk_zones = TRUE          # Optional
)

# Alternatively, plot pre-calculated data:
# plot_exposure(exposure_df = exposure_data)
```

![](https://gaudy-pipe-239.notion.site/image/attachment%3Ada869625-0481-4b1d-af1a-a1785add2962%3Aimage.png?table=block&id=1c9fc401-a191-8045-aadf-cc29956870ef&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

### 2. ACWR Trend

With this analysis, you can monitor the rate of change in your training load over time, helping to identify periods of rapid increases that might lead to overtraining or heightened injury risk. It's a valuable tool for periodized training monitoring and risk management. *Note: While `calculate_exposure` also calculates ACWR as part of its output, this `calculate_acwr` function focuses specifically on the ACWR metric and its associated plot.*

**Calculating Data:**
To get the underlying ACWR data (date, acwr, acwr_smooth) as a data frame:
```r
# Ensure stoken is valid
acwr_data <- calculate_acwr(
    stoken = stoken,
    activity_type = "Run",
    load_metric = "duration_mins",
    acute_period = 7, 
    chronic_period = 28
)
# print(tail(acwr_data)) # Uncomment to view
```

**Plotting:**
```r
# Ensure stoken is valid
plot_acwr(
    stoken = stoken,
    activity_type = "Run",         # Specify activity type(s), e.g., "Run", "Ride", NULL for all
    load_metric = "duration_mins", # Choose metric: "duration_mins", "distance_km", "elapsed_time_mins", "tss", "hrss", "elevation_gain_m"
    acute_period = 7,              # Duration (days) for acute load calculation
    chronic_period = 28            # Duration (days) for chronic load calculation
    # , start_date = NULL          # Optional: Start date (YYYY-MM-DD) for analysis (defaults to 1 year ago)
    # , end_date = NULL            # Optional: End date (YYYY-MM-DD) for analysis (defaults to today)
    # , user_ftp = NULL            # Optional: Required if load_metric = "tss"
    # , user_max_hr = NULL         # Optional: Required if load_metric = "hrss"
    # , user_resting_hr = NULL     # Optional: Required if load_metric = "hrss"
    # , smoothing_period = 7       # Optional: Days for smoothing the ACWR line (defaults to 7)
    # , highlight_zones = TRUE     # Optional: Show background risk zone shading (defaults to TRUE)
)

# Alternatively, plot pre-calculated data:
# plot_acwr(acwr_df = acwr_data)
```

![](https://gaudy-pipe-239.notion.site/image/attachment%3A3b50a271-b755-4eb5-9108-34f97e68b58b%3Aimage.png?table=block&id=1cafc401-a191-80e8-967a-fc60f6946af5&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

### 3. Efficiency Factor Trend

The Efficiency Factor (EF) is a common metric for assessing aerobic fitness adaptations. Ideally, EF should trend upwards as fitness improves, indicating higher output for the same physiological cost. This analysis helps track long-term aerobic efficiency progress and may signal accumulating fatigue or fluctuations in form.

**Calculating Data:**
To get the underlying EF data (date, activity_type, ef_value) as a data frame:
```r
# Ensure stoken is valid
ef_data <- calculate_ef(
    stoken = stoken,
    activity_type = c("Run", "Ride"), # Example: Runs and Rides
    ef_metric = "Pace_HR"             # Example: Pace/HR
)
# print(tail(ef_data)) # Uncomment to view
```

**Plotting:**
```r
# Ensure stoken is valid
plot_ef(
    stoken = stoken,
    activity_type = c("Run", "Ride"), # Specify activity type(s)
    ef_metric = "Pace_HR"             # Choose metric: "Pace_HR", "Power_HR"
    # , start_date = NULL             # Optional
    # , end_date = NULL               # Optional
    # , min_duration_mins = 20        # Optional
    # , add_trend_line = TRUE         # Optional
    # , smoothing_method = "loess"    # Optional
)

# Alternatively, plot pre-calculated data:
# plot_ef(ef_df = ef_data)
```

![](https://gaudy-pipe-239.notion.site/image/attachment%3A20a75be7-f255-43ce-a848-ad8e4213858e%3Aimage.png?table=block&id=1cdfc401-a191-806a-986a-d49ee4389a08&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

### 4. Long-Term Performance Metrics Tracking (Personal Bests - PBs)

This analysis allows you to visualize the progression of your key performance benchmarks over extended periods. Track metrics like your estimated best times for standard distances (e.g., 1k, 5k, 10k) or critical power outputs. By observing these trends, including highlighted Personal Bests (PBs), you can directly assess the effectiveness of different training blocks, validate fitness improvements beyond physiological markers, identify performance plateaus, and stay motivated by seeing concrete results align with your goals.

**Calculating Data:**
To get the underlying PB data (activity_id, activity_date, distance, time_seconds, cumulative_pb_seconds, is_pb, etc.) as a data frame:
```r
# Ensure stoken is valid
pb_data <- calculate_pbs(
    stoken = stoken,
    distance_meters = c(1000, 5000, 10000), # Example: 1k, 5k, 10k
    activity_type = "Run"                   # Example: For Runs
)
# print(tail(pb_data)) # Uncomment to view
# Filter for actual new PBs
# new_pbs <- pb_data[pb_data$is_pb, ]
# print(new_pbs)
```

**Plotting:**
```r
# Ensure stoken is valid
plot_pbs(
    stoken = stoken,
    distance_meters = c(1000, 5000, 10000), # Specify distances in meters (REQUIRED)
    activity_type = "Run"                   # Specify activity type (currently only "Run" is fully supported)
    # , max_activities = 100                  # Optional: Limit activities to check for speed/API limits
    # , date_range = NULL                   # Optional
)

# Alternatively, plot pre-calculated data:
# plot_pbs(pbs_df = pb_data)
```
![](https://gaudy-pipe-239.notion.site/image/attachment%3Af5624d35-ad3d-4242-aefc-7cf49881b777%3Aimage.png?table=block&id=1cbfc401-a191-808d-a62b-faa76e4beb5f&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

### 5. Decoupling Trend (Pace/Power vs HR)

**Note:** This analysis requires fetching detailed stream data for each activity (`calculate_decoupling` uses `httr` for this) and can be very slow or hit API rate limits. Use the `max_activities` parameter to limit the scope.

**Calculating Data:**
To get the underlying decoupling data (date, decoupling) as a data frame:
```r
# Ensure stoken is valid.

decoupling_data <- calculate_decoupling(
    stoken = stoken,
    activity_type = "Run",
    decouple_metric = "Pace_HR",
    max_activities = 20 # Use a small number for example
)

# Check the result (it might be NULL or stop with an error if issues occurred)
# print(tail(decoupling_data)) 
```

**Plotting:**
```r
# Ensure stoken is valid.
# WARNING: Can be slow. Reduce max_activities for plotting.

plot_decoupling(
    stoken = stoken,
    activity_type = "Run",
    decouple_metric = "Pace_HR",
    max_activities = 20 # Use a small number for example
)
# Alternatively, plot pre-calculated data (if 'decoupling_data' was successfully calculated earlier):
# plot_decoupling(decoupling_df = decoupling_data)
```

![](https://gaudy-pipe-239.notion.site/image/attachment%3A13491597-6762-4ea3-843d-13005cf21e8a%3Aimage.png?table=block&id=1cbfc401-a191-80b5-8f1a-efda0eddf069&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)


### Recent Updates

For details on recent changes and version history, please see the [Changelog](https://hezhiang.com/Athlytics/news/index.html) on the package website.

---

### Development Status & Potential Issues

Athlytics is under active development. While core functionalities are tested, variations in Strava data availability or unforeseen API interactions might occasionally lead to warnings or errors during execution. We appreciate bug reports and contributions to improve robustness!
