test_that("ESPN - Get ESPN NHL team roster", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_team_roster(team_id = "4")

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL team roster endpoint at test time")
  }

  key_cols <- c(
    "position_group",
    "id",
    "full_name",
    "display_name",
    "first_name",
    "last_name",
    "jersey",
    "position_name",
    "position_abbreviation",
    "weight",
    "height",
    "age",
    "status_name",
    "team_id"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(all(x$team_id == "4"))

  Sys.sleep(1)
})
