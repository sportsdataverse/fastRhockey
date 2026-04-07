test_that("NHL - Get NHL divisions info", {
  skip_on_cran()
  skip_nhl_test()
  x <- nhl_divisions_info(division_name = "Atlantic")

  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  expect_true("division_name" %in% names(x))
  expect_true("team_abbr" %in% names(x))
  expect_true("team_name" %in% names(x))
  expect_true(all(x$division_name == "Atlantic"))
})
