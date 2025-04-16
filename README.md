


# Athlytics 

<!--  --> <!-- Uncomment if on CRAN -->


[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN: Submitted](https://img.shields.io/badge/CRAN-Submitted-blue)](https://CRAN.R-project.org/) 
![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) 
[![pkgdown](https://img.shields.io/badge/pkgdown-site-blue.svg)](<in preparation>)
[![arXiv](https://img.shields.io/badge/arXiv-preprint-B31B1B.svg)](< in preparation >)
### Overview

This is the development repository for **Athlytics**, an R package designed to simplify the analysis of athletic performance and training load data sourced **directly from the Strava API**. The package provides functions for fetching your Strava data, calculating key metrics, and generating insightful visualizations for training monitoring.

### Installation

The package can be installed from CRAN (once available):

```r
install.packages('Athlytics')

```

The development version from this repository can be installed as follows (once available):

```r
# install.packages('remotes')
remotes::install_github('HzaCode/Athlytics')

```

You also need to install `rStrava` if you don't have it already:

```r
install.packages("rStrava")

```

### Using Athlytics Functions

```r
library(Athlytics)

```

### Authentication with Strava (using `rStrava`)

`Athlytics` **requires** you to authenticate with Strava using the `rStrava` package. This generates a token that you then pass to `Athlytics` functions.

1. **Create a Strava API Application:**
    - Go to https://www.strava.com/settings/api.
    - Create a new API application (e.g., "My Athlytics Analysis").
    - Set the "Authorization Callback Domain" to `localhost`.
    - Note your **Client ID** and **Client Secret**. **Keep the Secret confidential!**
2. **Authenticate in R using `rStrava::strava_oauth()`:**
    - This function handles the OAuth2.0 flow, potentially opening a browser window for authorization.
    - It returns a `Token2.0` object needed by `Athlytics`.
    - **Use `cache = TRUE`** (default) to store the token securely (in `.httr-oauth`), avoiding re-authentication in later sessions.

```r

library(Athlytics)
library(rStrava) 

# Sys.setenv(STRAVA_CLIENT_ID = "YOUR_CLIENT_ID")
# Sys.setenv(STRAVA_CLIENT_SECRET = "YOUR_SECRET")

# --- Authentication Step ---
app_name <- 'MyAthlyticsApp' # Choose a name
client_id <- Sys.getenv("STRAVA_CLIENT_ID")
client_secret <- Sys.getenv("STRAVA_CLIENT_SECRET")

# Authenticate using rStrava and STORE the token object
# Make sure app_scope allows reading activities.
stoken <- rStrava::strava_oauth(app_name,
                                client_id = client_id,
                                client_secret = client_secret,
                                app_scope = "activity:read_all", # Or specific scope needed
                                cache = TRUE) # IMPORTANT for reusing the token

```

### Optional Parameters for Specific Metrics

While most functions rely on data directly available from Strava (like duration, distance, heart rate), some advanced calculations or metrics **may require additional user-specific information**.

- `user_weight_kg` (Numeric): Your body weight in kilograms. Potentially used for power estimations or weight-adjusted metrics.
- `user_ftp` (Numeric): Your Functional Threshold Power (in Watts). Needed for calculating TSS or power zones for cycling/running with power.
- `user_max_hr` (Numeric): Your maximum heart rate.

*(Provide these parameters directly to the Athlytics function call if the chosen `load_metric` or analysis requires them. Check function documentation for details.)*

### Key Analysis Visualizations

These functions generate plots to analyze trends and performance, using data fetched and processed from Strava via the `stoken` you provide.

### 1. Load Exposure

This analysis provides an intuitive way to assess your current training load status and potential injury risk level. You can clearly see where your load combination (e.g., high acute load on high chronic load vs. high acute load on low chronic load) falls within defined risk zones (like the sweet spot, caution zone, or danger zone).

```r
plot_exposure(
    stoken = stoken,
    activity_type = "Ride",
    load_metric = "tss",
    user_ftp = 280,                 # REQUIRED: Provide your Functional Threshold Power for TSS calculation
    acute_period = 7,              
    chronic_period = 28            
)

```

![](https://gaudy-pipe-239.notion.site/image/attachment%3Ada869625-0481-4b1d-af1a-a1785add2962%3Aimage.png?table=block&id=1c9fc401-a191-8045-aadf-cc29956870ef&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

### 2. ACWR Trend

With this analysis, you can monitor the rate of change in your training load over time, helping to identify periods of rapid increases that might lead to overtraining or heightened injury risk. It's a valuable tool for periodized training monitoring and risk management.

```r
plot_acwr(
    stoken = stoken,
    activity_type = "Run",
    load_metric = "duration_mins", # Choose metric
    acute_period = 7,              # Optional: Define acute window
    chronic_period = 28            # Optional: Define chronic window
)

```

![](https://gaudy-pipe-239.notion.site/image/attachment%3A3b50a271-b755-4eb5-9108-34f97e68b58b%3Aimage.png?table=block&id=1cafc401-a191-80e8-967a-fc60f6946af5&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

### 3. Efficiency Factor Trend

The Efficiency Factor (EF) is a common metric for assessing aerobic fitness adaptations. Ideally, EF should trend upwards as fitness improves, indicating higher output for the same physiological cost. This analysis helps track long-term aerobic efficiency progress and may signal accumulating fatigue or fluctuations in form.

```r
plot_ef(
    stoken = stoken,
    activity_type = c("Run", "Ride")
    ef_metric = "Pace_HR"         # Choose metric: "Pace_HR", "Power_HR"
)

```

![](https://gaudy-pipe-239.notion.site/image/attachment%3A20a75be7-f255-43ce-a848-ad8e4213858e%3Aimage.png?table=block&id=1cdfc401-a191-806a-986a-d49ee4389a08&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

### 4. Long-Term Performance Metrics Tracking

This analysis allows you to visualize the progression of your key performance benchmarks over extended periods. Track metrics like your estimated best times for standard distances (e.g., 1k, 5k, 10k) or critical power outputs. By observing these trends, including highlighted Personal Bests (PBs), you can directly assess the effectiveness of different training blocks, validate fitness improvements beyond physiological markers, identify performance plateaus, and stay motivated by seeing concrete results align with your goals.

```r
plot_pbs(
    stoken = stoken,
    activity_type = "Run",
    distance_meters = c(1000, 5000, 10000) # Specify distances
)

```

![](https://gaudy-pipe-239.notion.site/image/attachment%3Af5624d35-ad3d-4242-aefc-7cf49881b777%3Aimage.png?table=block&id=1cbfc401-a191-808d-a62b-faa76e4beb5f&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

### 5. Pace/Power vs. Heart Rate Decoupling

This analysis quantifies how much your heart rate "drifts" upwards relative to a steady pace or power output during aerobic activities. Calculated typically by comparing efficiency (Pace/HR or Power/HR) in the first half versus the second half of a sustained effort, a lower decoupling rate generally indicates better aerobic endurance and cardiovascular fitness. By categorizing runs based on duration, this view helps identify if endurance capacity differs across varying effort lengths and tracks improvements over time. Monitoring this trend can reveal stamina gains, signal fatigue/dehydration, or evaluate pacing strategies.

```r
plot_decoupling(
    stoken = stoken,
    activity_type = "Run",
    decouple_metric = "Pace_HR"      # Choose metric: "Pace_HR", "Power_HR"
)

```

![](https://gaudy-pipe-239.notion.site/image/attachment%3A13491597-6762-4ea3-843d-13005cf21e8a%3Aimage.png?table=block&id=1cbfc401-a191-80b5-8f1a-efda0eddf069&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

---

*README actively updating...*
