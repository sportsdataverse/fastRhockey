test_that("ESPN - Get ESPN NHL PBP", {
  skip_on_cran()
  skip_espn_test()

  sb  <- espn_nhl_scoreboard(dates = "20250110")
  if (!is.data.frame(sb) || nrow(sb) == 0) {
    skip("No scoreboard rows returned at test time")
  }
  gid <- sb$game_id[1]

  x <- espn_nhl_pbp(game_id = gid)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL PBP endpoint at test time")
  }

  key_cols <- c(
    "id",
    "sequence_number",
    "type_id",
    "type_text",
    "text",
    "away_score",
    "home_score",
    "scoring_play",
    "period_number",
    "clock_display_value",
    "game_id",
    "season",
    "home_team_id",
    "home_team_abbrev",
    "away_team_id",
    "away_team_abbrev"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  expect_true(all(x$game_id == as.character(gid)))

  Sys.sleep(1)
})
