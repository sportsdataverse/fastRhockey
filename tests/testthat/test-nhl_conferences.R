test_that("NHL - Get NHL conferences", {
  skip_on_cran()
  x <- nhl_conferences()

  cols <- c(
    "conference_id", "name", "link", "abbreviation", "short_name", "active"
  )
  expect_equal(colnames(x), cols)
  expect_s3_class(x, 'data.frame')

})
