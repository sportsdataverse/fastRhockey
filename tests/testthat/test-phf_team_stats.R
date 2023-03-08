test_that("phf_team_roster", {
  skip_on_cran()
  cols_1 <- c(
    "team_id",
    "team_name",
    "group",
    "division_id",
    "player_jersey",
    "player_name",
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
    "skaters_href",
    "player_id"
  )
  cols_2 <- c(
    "team_id",
    "team_name",
    "group",
    "division_id",
    "player_jersey",
    "player_name",
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
    "games_started",
    "season_type",
    "goalies_href",
    "player_id"
  )
  x <- phf_team_stats(team = "Boston Pride", season = 2022)

  expect_equal(colnames(x$skaters), cols_1)
  expect_equal(colnames(x$goalies), cols_2)

  expect_s3_class(x$skaters, "data.frame")
  expect_s3_class(x$goalies, "data.frame")
})
