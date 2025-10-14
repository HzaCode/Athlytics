# Test parse_activity_file functionality

library(testthat)
library(Athlytics)

# Skip all tests in this file - parse_activity_file and related functions are internal
# They are tested indirectly through calculate_pbs, calculate_decoupling, etc.

test_that("parse functions are tested via calculate functions", {
  skip("parse_activity_file and related functions are not exported - tested indirectly via calculate functions")
})
