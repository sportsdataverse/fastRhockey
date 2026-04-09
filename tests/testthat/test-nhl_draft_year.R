test_that("NHL - Get NHL Draft by Year", {
  skip_on_cran()
  skip_nhl_test()
  x <- nhl_draft_year(year = 2023)

  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
})

test_that("NHL - Get NHL Draft by Year with Round", {
  skip_on_cran()
  skip_nhl_test()
  x <- nhl_draft_year(year = 2023, round = 1)

  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
})

test_that("NHL - Get NHL Draft by Year with round = 'all'", {
  skip_on_cran()
  skip_nhl_test()
  x <- nhl_draft_year(year = 2023, round = "all")

  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
})
