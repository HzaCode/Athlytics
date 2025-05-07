# Athlytics 0.1.1

## Core Improvement: Enhanced Reliability & Testing with Simulated Data

This significant update enhances package reliability and ease of use by integrating `Athlytics_sample_data`. This enables all examples to run offline and ensures core functionalities have undergone more rigorous, reproducible testing.

### Key Changes:

*   **Examples & Vignettes**: All Roxygen examples and key vignette examples now primarily use `Athlytics_sample_data` for offline execution and clarity. Network-dependent examples are clearly separated in `\donttest{}` blocks.
*   **Test Suite**: Fundamentally refactored the test suite to extensively use `Athlytics_sample_data` and `mockery`, improving test robustness and parameter coverage.
*   **Strengthened Package Quality & Compliance**: Undertook thorough package validation, leading to key enhancements for overall robustness and adherence to R packaging standards. This involved: ensuring all **function examples** are correct and reliably executable (notably addressing `strava_oauth(...)` scenarios for offline/testing contexts); providing accurate and **refined documentation for data objects** in `R/data.R`; fixing **Roxygen import directives** for precise namespace definition; improving **help file readability** through Rd line width adjustments; and optimizing package data loading by adding `LazyData: true` to `DESCRIPTION`.
*   **Documentation**: Minor improvements to documentation clarity and consistency (e.g., date formatting in plots, explicit naming of data frame arguments in examples).


---
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

## Initial Release

* Initial release.
* Added core functions for calculating and plotting:
    * Load Exposure (Acute vs. Chronic Load)
    * ACWR Trend
    * Efficiency Factor Trend
    * Personal Bests (PBs)
    * Decoupling Trend
* Added Strava authentication helper based on `rStrava`.