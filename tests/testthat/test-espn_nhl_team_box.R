test_that("ESPN - Get ESPN NHL Team Box Score", {
  skip_on_cran()
  skip_espn_test()

  sb  <- espn_nhl_scoreboard(dates = "20250110")
  if (!is.data.frame(sb) || nrow(sb) == 0) {
    skip("No scoreboard rows returned at test time")
  }
  gid <- sb$game_id[1]

  x <- espn_nhl_team_box(game_id = gid)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL team box endpoint at test time")
  }

  key_cols <- c(
    "team_id",
    "team_abbreviation",
    "team_display_name",
    "home_away",
    "game_id",
    "season",
    "season_type",
    "game_date",
    "blocked_shots",
    "hits",
    "shots_total",
    "power_play_goals",
    "power_play_opportunities",
    "giveaways",
    "penalties",
    "penalty_minutes"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 2L)
  expect_true(all(x$game_id == as.character(gid)))

  Sys.sleep(1)
})
