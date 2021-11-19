test_that("NHL - Get NHL draft", {
  skip_on_cran()
  x <- nhl_draft()
  
  cols <- c(
    "year",
    "round",
    "pick_overall",
    "pick_in_round",
    "team_id",
    "team_name",
    "team_link",
    "prospect_id",
    "prospect_full_name",
    "prospect_link"
  )
  expect_equal(colnames(x), cols)
  expect_s3_class(x, 'data.frame')

})
