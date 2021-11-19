test_that("NHL - Get NHL Game Shifts", {
  skip_on_cran()
  x <- nhl_game_shifts(game_id=2021020182)
  
  cols <- c("event_team",
            "period",
            "period_time",
            "period_seconds",
            "game_seconds",
            "num_on",
            "players_on",
            "num_off",
            "players_off",
            "event",
            "event_type",
            "game_seconds_remaining")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, 'data.frame')
  
})
