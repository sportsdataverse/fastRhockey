test_that("phf_player_stats", {
  skip_on_cran()
  cols_1 <- c(
    "season",
    "team_name",
    "division",
    "GP",
    "W",
    "L",
    "T",
    "OTL",
    "minutes_played",
    "shots_against",
    "goals_against",
    "save_percent",
    "GAA",
    "SO",
    "saves",
    "penalty_minutes",
    "goals",
    "assists",
    "GS",
    "player_name",
    "player_id",
    "position",
    "player_image_href",
    "team_href",
    "season_type",
    "team_id"
  )
  cols_2 <- c(
    "date",
    "game",
    "shots_against",
    "goals_against",
    "saves",
    "save_percent",
    "minutes_played",
    "penalty_minutes",
    "goals",
    "assists",
    "Pos",
    "player_name",
    "player_id",
    "position",
    "player_image_href",
    "game_href",
    "game_id"
  )
  cols_3 <- c(
    "season",
    "team_name",
    "division",
    "games_played",
    "goals",
    "assists",
    "points",
    "shots_on_goal",
    "faceoffs_won_lost",
    "faceoffs_won",
    "faceoffs_lost",
    "blocks",
    "penalty_minutes",
    "takeaways",
    "giveaways",
    "scoring_pct",
    "powerplay_goals",
    "shorthanded_goals",
    "game_winning_goals",
    "points_per_game_average",
    "faceoffs_win_pct",
    "shots_blocked",
    "shots",
    "player_name",
    "player_id",
    "position",
    "player_image_href",
    "team_href",
    "season_type",
    "team_id"
  )
  cols_4 <- c(
    "date",
    "game",
    "Pos",
    "goals",
    "assists",
    "points",
    "penalty_minutes",
    "shots_on_goal",
    "blocks",
    "giveaways",
    "takeaways",
    "faceoffs_won_lost",
    "faceoffs_won",
    "faceoffs_lost",
    "faceoffs_win_pct",
    "PPG",
    "SHG",
    "shots",
    "shots_blocked",
    "FoW",
    "FoL",
    "player_name",
    "player_id",
    "position",
    "player_image_href",
    "game_href",
    "game_id"
  )
  x <- phf_player_stats(player_id = 431611)
  y <- phf_player_stats(player_id = 532475)

  expect_equal(colnames(x$career), cols_1)
  expect_equal(colnames(x$game_log), cols_2)
  expect_equal(colnames(y$career), cols_3)
  expect_equal(colnames(y$game_log), cols_4)
  expect_s3_class(x$career, "data.frame")
  expect_s3_class(x$game_log, "data.frame")
  expect_s3_class(y$career, "data.frame")
  expect_s3_class(y$game_log, "data.frame")
})
