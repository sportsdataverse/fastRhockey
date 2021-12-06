test_that("NHL - Get NHL conferences info", {
  skip_on_cran()
  x <- nhl_conferences_info(conference_id = 7)

  cols <- c(
    "conference_id", "name", "link", "abbreviation", "short_name", "active"
  )
  expect_equal(colnames(x), cols)
  expect_s3_class(x, 'data.frame')

})
