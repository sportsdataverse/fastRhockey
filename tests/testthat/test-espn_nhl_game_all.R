test_that("ESPN - Get ESPN NHL Game All (plays + team_box + player_box)", {
  skip_on_cran()
  skip_espn_test()

  sb  <- espn_nhl_scoreboard(dates = "20250110")
  if (!is.data.frame(sb) || nrow(sb) == 0) {
    skip("No scoreboard rows returned at test time")
  }
  gid <- sb$game_id[1]

  g <- espn_nhl_game_all(game_id = gid)

  # Must be a named list with the three expected components
  expect_type(g, "list")
  expect_named(g, c("plays", "team_box", "player_box"), ignore.order = FALSE)

  # Skip-if-empty guards for each component
  if (!is.data.frame(g$plays) || nrow(g$plays) == 0) {
    skip("No plays returned from ESPN NHL game_all endpoint at test time")
  }
  if (!is.data.frame(g$team_box) || nrow(g$team_box) == 0) {
    skip("No team_box rows returned from ESPN NHL game_all at test time")
  }
  if (!is.data.frame(g$player_box) || nrow(g$player_box) == 0) {
    skip("No player_box rows returned from ESPN NHL game_all at test time")
  }

  # plays
  expect_s3_class(g$plays, "data.frame")
  expect_true(nrow(g$plays) > 0)
  expect_in(c("id", "type_text", "period_number", "game_id"),
            colnames(g$plays))

  # team_box
  expect_s3_class(g$team_box, "data.frame")
  expect_equal(nrow(g$team_box), 2L)
  expect_in(c("team_id", "team_abbreviation", "home_away", "game_id"),
            colnames(g$team_box))

  # player_box
  expect_s3_class(g$player_box, "data.frame")
  expect_true(nrow(g$player_box) > 0)
  expect_in(c("athlete_id", "stat_group", "team_id", "game_id"),
            colnames(g$player_box))

  Sys.sleep(1)
})
