
# Athlytics <img src="man/figures/logo.png" align="right" height="138" /> <!-- Optional: Add your logo path -->


[![CRAN status](<https://www.r-pkg.org/badges/version/Athlytics>)](<https://CRAN.R-project.org/package/Athlytics>)
[![Lifecycle: experimental](<https://img.shields.io/badge/lifecycle-experimental-orange.svg>)](<https://lifecycle.r-lib.org/articles/stages.html#experimental>) <!-- Adjust lifecycle stage -->
<!-- Add other badges like R-CMD-check if you have CI set up -->
<!-- [![](<http://cranlogs.r-pkg.org/badges/grand-total/Athlytics>)](<https://cran.rstudio.com/web/packages/Athlytics/index.html>) --> <!-- Uncomment if on CRAN -->


### Overview and installation

This is the development repository for **Athlytics**, an R package designed to simplify the analysis of athletic performance and training load data sourced **directly from the Strava API**. The package provides functions for fetching your Strava data, calculating key metrics, and generating insightful visualizations for training monitoring.

The package can be installed from CRAN (once available):

``` r
install.packages('Athlytics')
```
The development version from this repository can be installed as follows (once available):
``` r
# install.packages('remotes')
remotes::install_github('HzaCode/Athlytics')
```

### Using Athlytics Functions
 ``` r
library(Athlytics)
```

### Key Analysis Visualizations 

These functions generate plots to analyze trends and performance, using data fetched and processed from Strava.

### 1. Load Exposure

This analysis provides an intuitive way to assess your current training load status and potential injury risk level. You can clearly see where your load combination (e.g., high acute load on high chronic load vs. high acute load on low chronic load) falls within defined risk zones (like the sweet spot, caution zone, or danger zone).

``` r
plot_load_exposure()
```
![](https://gaudy-pipe-239.notion.site/image/attachment%3Ada869625-0481-4b1d-af1a-a1785add2962%3Aimage.png?table=block&id=1c9fc401-a191-8045-aadf-cc29956870ef&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

### 2. ACWR Trend

With this analysis, you can monitor the rate of change in your training load over time, helping to identify periods of rapid increases that might lead to overtraining or heightened injury risk. It's a valuable tool for periodized training monitoring and risk management.
``` r
plot_acwr_trend()
```
![](https://gaudy-pipe-239.notion.site/image/attachment%3A3b50a271-b755-4eb5-9108-34f97e68b58b%3Aimage.png?table=block&id=1cafc401-a191-80e8-967a-fc60f6946af5&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

### 3. Efficiency Factor Trend

The Efficiency Factor (EF) is a common metric for assessing aerobic fitness adaptations. Ideally, EF should trend upwards as fitness improves, indicating higher output for the same physiological cost. This analysis helps track long-term aerobic efficiency progress and may signal accumulating fatigue or fluctuations in form.
``` r
plot_ef()
```
![](https://gaudy-pipe-239.notion.site/image/attachment%3A6bc4a180-e60c-414e-844a-a898efd42876%3Aimage.png?table=block&id=1cafc401-a191-8056-b314-fabc3d3d44ac&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1360&userId=&cache=v2)

### 4. Long-Term Performance Metrics Tracking

This analysis allows you to visualize the progression of your key performance benchmarks over extended periods. Track metrics like your estimated best times for standard distances (e.g., 1k, 5k, 10k) or critical power outputs. By observing these trends, including highlighted Personal Bests (PBs), you can directly assess the effectiveness of different training blocks, validate fitness improvements beyond physiological markers, identify performance plateaus, and stay motivated by seeing concrete results align with your goals. 
``` r
plot_pb_progression() 
```
![](https://gaudy-pipe-239.notion.site/image/attachment%3Af5624d35-ad3d-4242-aefc-7cf49881b777%3Aimage.png?table=block&id=1cbfc401-a191-808d-a62b-faa76e4beb5f&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)
### 5. Pace/Power vs. Heart Rate Decoupling

This analysis quantifies how much your heart rate "drifts" upwards relative to a steady pace or power output during aerobic activities. Calculated typically by comparing efficiency (Pace/HR or Power/HR) in the first half versus the second half of a sustained effort, a lower decoupling rate generally indicates better aerobic endurance and cardiovascular fitness. By categorizing runs based on duration, this view helps identify if endurance capacity differs across varying effort lengths and tracks improvements over time. Monitoring this trend can reveal stamina gains, signal fatigue/dehydration, or evaluate pacing strategies.
``` r
plot_decoupling()
```
![](https://gaudy-pipe-239.notion.site/image/attachment%3A13491597-6762-4ea3-843d-13005cf21e8a%3Aimage.png?table=block&id=1cbfc401-a191-80b5-8f1a-efda0eddf069&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)
### 6
---
*README actively updating...*
