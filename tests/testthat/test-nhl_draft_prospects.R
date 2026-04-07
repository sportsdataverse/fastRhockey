test_that("NHL - Get NHL Draft Prospects", {
  skip_on_cran()
  skip_nhl_test()
  x <- nhl_draft_prospects()

  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
})
