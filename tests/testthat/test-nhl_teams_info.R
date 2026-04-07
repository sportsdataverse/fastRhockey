test_that("NHL - Get NHL Teams Info", {
  skip_on_cran()
  skip_nhl_test()
  x <- nhl_teams_info(team_abbr = "TBL")

  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  expect_true("team_abbr" %in% names(x))
  expect_true("team_name" %in% names(x))
  expect_equal(x$team_abbr[1], "TBL")
})
