test_that("ESPN - Get ESPN NHL teams", {
  skip_on_cran()
  skip_nhl_test()
  x <- espn_nhl_teams()

  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)

  # Check key columns exist (ESPN may add/remove logo columns over time)
  key_cols <- c(
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
    "logo_dark"
  )
  for (col in key_cols) {
    expect_true(col %in% names(x), info = paste("Missing column:", col))
  }
})
