test_that("NHL - Get NHL conferences", {
  skip_on_cran()
  skip_nhl_test()
  x <- nhl_conferences()

  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  expect_true("conference_name" %in% names(x))
  # NHL has Eastern and Western conferences
  expect_true(nrow(x) == 2)
})
