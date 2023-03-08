
test_that("phf_player_box", {
  skip_on_cran()
  skip_on_ci()
  cols_1 <- c(
    "player_jersey",
    "player_name",
    "position",
    "goals",
    "assists",
    "points",
    "penalty_minutes",
    "shots_on_goal",
    "blocks",
    "giveaways",
    "takeaways",
    "faceoffs_won_lost",
    "faceoffs_win_pct",
    "powerplay_goals",
    "shorthanded_goals",
    "shots",
    "shots_blocked",
    "faceoffs_won",
    "faceoffs_lost",
    "team",
    "skaters_href",
    "player_id",
    "game_id"
  )
  cols_2 <- c(
    "player_jersey",
    "player_name",
    "shots_against",
    "goals_against",
    "saves",
    "save_percent",
    "minutes_played",
    "penalty_minutes",
    "goals",
    "assists",
    "team",
    "goalies_href",
    "player_id",
    "game_id"
  )
  x <- phf_player_box(game_id = 40863)
  y <- phf_player_box(game_id = 268079)

  expect_equal(colnames(x$skaters), cols_1)
  expect_equal(colnames(x$goalies), cols_2)
  expect_equal(colnames(y$skaters), cols_1)
  expect_equal(colnames(y$goalies), cols_2)
  expect_s3_class(x$skaters, "data.frame")
  expect_s3_class(x$goalies, "data.frame")
  expect_s3_class(y$skaters, "data.frame")
  expect_s3_class(y$goalies, "data.frame")


})
