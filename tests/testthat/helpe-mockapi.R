# tests/testthat/helper-mockapi.R

with_mocked_strava_api <- function(expr, mock_data = mock_activity_list_df) {
  mockery::stub(
    where = rStrava::get_activity_list,
    what = "rStrava::get_activity_list",
    how = mock_data
  )
  eval(expr)
}
