test_that("NHL - Get NHL draft by year", {
  skip_on_cran()
  x <- nhl_draft_year(year=2020)

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
  x <- x %>% dplyr::select(dplyr::all_of(cols))
  expect_equal(colnames(x), cols)
  expect_s3_class(x, 'data.frame')

})
