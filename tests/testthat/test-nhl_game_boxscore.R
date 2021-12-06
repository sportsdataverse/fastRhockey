test_that("NHL - Get NHL Game Boxscore", {
  skip_on_cran()
  x <- nhl_game_boxscore(game_id=2021020182)
  
  names <- c("team_box", 
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
