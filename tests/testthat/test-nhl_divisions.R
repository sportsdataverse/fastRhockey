test_that("NHL - Get NHL divisions", {
  skip_on_cran()
  x <- nhl_divisions()

  cols <- c(
    "division_id",
    "name",
    "name_short",
    "link",
    "abbreviation",
    "active",
    "conference_id",
    "conference_name",
    "conference_link"
  )
  expect_equal(colnames(x), cols)
  expect_s3_class(x, 'data.frame')

})
