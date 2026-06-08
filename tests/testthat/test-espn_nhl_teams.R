test_that("ESPN - Get ESPN NHL teams", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_teams()

  # Skip-if-empty guard
  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL teams endpoint at test time")
  }

  # Subset-direction: expected columns must be present in actual columns
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
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 30)

  Sys.sleep(1)
})
