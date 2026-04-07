test_that("NHL - Get NHL divisions", {
  skip_on_cran()
  skip_nhl_test()
  x <- nhl_divisions()

  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  expect_true("division_name" %in% names(x))
  expect_true("division_abbrev" %in% names(x))
  expect_true("conference_name" %in% names(x))
  # NHL has 4 divisions: Atlantic, Metropolitan, Central, Pacific
  expect_true(nrow(x) == 4)
})
