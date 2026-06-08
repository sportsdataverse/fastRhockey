test_that("ESPN - Get ESPN NHL Player Box Score", {
  skip_on_cran()
  skip_espn_test()

  sb  <- espn_nhl_scoreboard(dates = "20250110")
  if (!is.data.frame(sb) || nrow(sb) == 0) {
    skip("No scoreboard rows returned at test time")
  }
  gid <- sb$game_id[1]

  x <- espn_nhl_player_box(game_id = gid)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL player box endpoint at test time")
  }

  key_cols <- c(
    "athlete_id",
    "athlete_display_name",
    "athlete_jersey",
    "athlete_position_name",
    "athlete_position_abbreviation",
    "stat_group",
    "team_id",
    "team_abbreviation",
    "team_display_name",
    "home_away",
    "team_score",
    "team_winner",
    "opponent_team_id",
    "game_id",
    "season",
    "season_type",
    "game_date"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  expect_true(all(x$game_id == as.character(gid)))

  # Both skater and goalie stat groups should be present
  expect_true("goalies" %in% x$stat_group)
  expect_true(any(x$stat_group %in% c("forwards", "defenses")))

  Sys.sleep(1)
})
