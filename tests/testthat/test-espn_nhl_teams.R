test_that("ESPN - Get ESPN NHL teams", {
  skip_on_cran()
  x <- espn_nhl_teams()

  cols <- c(
    "espn_team_id",
    "abbreviation",
    "display_name",
    "short_name",
    "mascot",
    "nickname",
    "team",
    "color",
    "alternate_color",
    "logo",
    "logo_dark",
    "logos_href_3",
    "logos_href_4"
  )
  expect_equal(colnames(x), cols)
  expect_s3_class(x, 'data.frame')

})
