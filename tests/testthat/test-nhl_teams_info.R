test_that("NHL - Get NHL Teams Info", {
  skip_on_cran()
  x <- nhl_teams_info(team_id = 14)
  
  cols <- c("team_id",
            "name",
            "link",
            "abbreviation",
            "team_name",
            "location_name",
            "first_year_of_play",
            "short_name",
            "official_site_url",
            "franchise_id",
            "active",
            "venue_name",
            "venue_link",
            "venue_city",
            "venue_time_zone_id",
            "venue_time_zone_offset",
            "venue_time_zone_tz",
            "division_id",
            "division_name",
            "division_name_short",
            "division_link",
            "division_abbreviation",
            "conference_id",
            "conference_name",
            "conference_link",
            "franchise_franchise_id",
            "franchise_team_name",
            "franchise_link")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, 'data.frame')
  
})
