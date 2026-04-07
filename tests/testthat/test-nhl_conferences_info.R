test_that("NHL - Get NHL conferences info", {
  skip_on_cran()
  skip_nhl_test()
  x <- nhl_conferences_info(conference_name = "Eastern")

  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  expect_true("conference_name" %in% names(x))
  expect_true("team_abbr" %in% names(x))
  expect_true("team_name" %in% names(x))
  expect_true(all(x$conference_name == "Eastern"))
})
