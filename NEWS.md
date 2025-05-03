# Athlytics 0.1.0

## Major Changes

*   **Decoupling Calculation**: Switched from `rStrava::get_activity_streams` to direct Strava API calls using `httr` and `jsonlite` for fetching activity streams in `calculate_decoupling`. This aims to resolve previous errors but might impact performance and rate limiting.

## Bug Fixes & Improvements

*   Fixed `calculate_acwr` error (`condition has length > 1`) by forcing evaluation before the dplyr pipe.
*   Corrected `plot_pbs` usage in examples and test scripts to include the required `distance_meters` argument.
*   Added missing dependencies (`httr`, `jsonlite`) to `DESCRIPTION` file.
*   Improved error handling and messages in several functions.
*   Simplified Roxygen documentation for core functions.
*   Updated README examples and descriptions for clarity and consistency with code.

## Previous Notes (Initial Release)

* Initial release.
* Added core functions for calculating and plotting:
    * Load Exposure (Acute vs. Chronic Load)
    * ACWR Trend
    * Efficiency Factor Trend
    * Personal Bests (PBs)
    * Decoupling Trend
* Added Strava authentication helper based on `rStrava`. 