test_that("NHL - Get NHL Schedule", {
  skip_on_cran()
  x <- nhl_schedule(season=2021)
  
  cols <- c("game_id",
            "link",
            "game_type_abbreviation",
            "season_full",
            "game_date_time",
            "status_abstract_game_state",
            "status_coded_game_state",
            "status_detailed_state",
            "status_status_code",
            "status_start_time_tbd",
            "away_score",
            "away_team_id",
            "away_team_name",
            "away_team_link",
            "home_score",
            "home_team_id",
            "home_team_name",
            "home_team_link",
            "venue_name",
            "venue_link",
            "venue_id",
            "content_link",
            "game_type",
            "game_date")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, 'data.frame')
  
})
