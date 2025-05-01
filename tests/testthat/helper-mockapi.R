# tests/testthat/helper-mockapi.R

# Basic placeholder for mocking Strava API calls
# In a real scenario, this would use packages like mockery or testthat::with_mock
# to intercept calls to rStrava functions (e.g., get_activity_list, get_activity)
# and return predefined mock data.

with_mocked_strava_api <- function(expr) {
  # Currently, this doesn't actually mock anything.
  # It just evaluates the expression.
  # To make tests pass that rely on API data, this function needs
  # to be implemented using mocking techniques.
  # For now, it allows tests using this wrapper to run without erroring
  # on the wrapper function itself.
  eval(expr)
} 