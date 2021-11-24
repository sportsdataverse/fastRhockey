test_that("NHL - Get NHL Teams Roster", {
  skip_on_cran()
  x <- nhl_teams_roster(team_id=14)

  cols <- c("jersey_number",
            "player_id",
            "player_full_name",
            "player_link",
            "position_code",
            "position_name",
            "position_type",
            "position_abbreviation",
            "team_id",
            "season")

  expect_equal(colnames(x), cols)
  expect_s3_class(x, 'data.frame')

})
