
test_that("phf_leaders", {
  skip_on_cran()
  skip_on_ci()
  cols_1 <- c(
    "player_jersey",
    "player_name",
    "team_name",
    "position",
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
    "season_type",
    "player_href",
    "team_href",
    "player_id",
    "team_id"
  )
  cols_2 <- c(
    "player_jersey",
    "player_name",
    "team_name",
    "games_played",
    "wins",
    "losses",
    "ties",
    "overtime_losses",
    "minutes_played",
    "shots_against",
    "goals_against",
    "save_percent",
    "goals_against_average",
    "shutouts",
    "saves",
    "penalty_minutes",
    "goals",
    "assists",
    "GS",
    "season_type",
    "player_href",
    "team_href",
    "player_id",
    "team_id"
  )
  x <- phf_leaders(player_type = "skaters", season = 2022, season_type="Regular Season")
  y <- phf_leaders(player_type = "goalies", season = 2022, season_type="Regular Season")

  expect_equal(colnames(x), cols_1)
  expect_equal(colnames(y), cols_2)
  expect_s3_class(x, "data.frame")
  expect_s3_class(y, "data.frame")


})
