test_that("NHL - Get NHL Game Feed", {
  skip_on_cran()
  x <- nhl_game_feed(game_id = 2021020182)

  names <- c("all_plays",
            "scoring_plays",
            "penalty_plays",
            "plays_by_period",
            "current_play",
            "linescore",
            "decisions",
            "team_box",
            "player_box",
            "skaters",
            "goalies",
            "on_ice",
            "on_ice_plus",
            "penalty_box",
            "scratches",
            "team_coaches")

  expect_equal(names(x), names)
  # expect_s3_class(x, 'data.frame')

})
