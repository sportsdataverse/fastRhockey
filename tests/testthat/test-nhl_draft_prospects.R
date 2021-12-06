test_that("NHL - Get NHL draft prospects", {
  skip_on_cran()
  x <- nhl_draft_prospects()
  
  cols <- c(
    "prospect_id",
    "full_name",
    "link",
    "first_name",
    "last_name",
    "birth_date",
    "birth_city",
    "birth_state_province",
    "birth_country",
    "height",
    "weight",
    "shoots_catches",
    "nhl_player_id",
    "draft_status",
    "nationality",
    "primary_position_code",
    "primary_position_name",
    "primary_position_type",
    "primary_position_abbreviation",
    "prospect_category_id",
    "prospect_category_short_name",
    "prospect_category_name",
    "amateur_team_name",
    "amateur_team_link",
    "amateur_league_name",
    "amateur_league_link",
    "ranks_final_rank",
    "ranks_draft_year"
  )
  expect_equal(colnames(x), cols)
  expect_s3_class(x, 'data.frame')

})
