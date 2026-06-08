test_that("ESPN - Get ESPN NHL standings", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_standings(season = 2025)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL standings endpoint at test time")
  }

  key_cols <- c(
    "group_name",
    "group_abbreviation",
    "team_id",
    "team_name",
    "team_abbreviation",
    "team_display_name",
    "team_location",
    "season",
    "wins",
    "losses",
    "points"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 30)
  expect_true(all(x$season == 2025))

  Sys.sleep(1)
})
