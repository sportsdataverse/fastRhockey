test_that("ESPN - Get ESPN NHL calendar", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_calendar()

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL calendar endpoint at test time")
  }

  key_cols <- c(
    "date",
    "calendar_type",
    "calendar_start_date",
    "calendar_end_date",
    "calendar_is_whitelist"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1)

  Sys.sleep(1)
})
