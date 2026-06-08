test_that("ESPN - Get ESPN NHL team schedule", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_schedule(team_id = "4", season = 2025)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL schedule endpoint at test time")
  }

  key_cols <- c(
    "game_id",
    "date",
    "name",
    "short_name",
    "season_year",
    "season_type",
    "status_type_name",
    "status_type_state",
    "home_id",
    "home_name",
    "home_abbreviation",
    "away_id",
    "away_name",
    "away_abbreviation",
    "team_id",
    "season"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(all(x$team_id == "4"))
  expect_true(all(x$season == 2025))

  Sys.sleep(1)
})
