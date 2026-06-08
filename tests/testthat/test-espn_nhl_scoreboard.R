test_that("ESPN - Get ESPN NHL scoreboard", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_scoreboard(dates = "20250110")

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL scoreboard endpoint at test time")
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
    "status_type_completed",
    "home_id",
    "home_name",
    "home_abbreviation",
    "home_score",
    "away_id",
    "away_name",
    "away_abbreviation",
    "away_score"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")

  Sys.sleep(1)
})
