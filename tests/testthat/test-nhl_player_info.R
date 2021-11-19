test_that("NHL - Get NHL Player Info", {
  skip_on_cran()
  x <- nhl_player_info(player_id=8476899)
  
  cols <- c("player_id",
            "full_name",
            "link",
            "first_name",
            "last_name",
            "primary_number",
            "birth_date",
            "current_age",
            "birth_city",
            "birth_state_province",
            "birth_country",
            "nationality",
            "height",
            "weight",
            "active",
            "alternate_captain",
            "captain",
            "rookie",
            "shoots_catches",
            "roster_status",
            "current_team_id",
            "current_team_name",
            "current_team_link",
            "primary_position_code",
            "primary_position_name",
            "primary_position_type",
            "primary_position_abbreviation")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, 'data.frame')
  
})
