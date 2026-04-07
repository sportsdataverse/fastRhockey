test_that("NHL - Get NHL Draft Prospects Info", {
  skip_on_cran()
  skip_nhl_test()
  x <- nhl_draft_prospects_info(year = 2025, prospect_category = 1)

  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
})
