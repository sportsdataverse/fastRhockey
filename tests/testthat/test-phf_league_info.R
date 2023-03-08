test_that("phf_league_info", {
  skip_on_cran()
  cols_seasons <- c(
    "id",
    "name",
    "type",
    "view_settings",
    "league_id"
  )
  cols_divisions <- c(
    "id",
    "name",
    "season_id",
    "tournament_id"
  )

  cols_teams <- c(
    "id",
    "name",
    "group",
    "division_id"
  )
  cols_officials <- c(
    "id",
    "name"
  )
  cols_brackets <- c(
    "id",
    "name"
  )
  x <- phf_league_info(season = 2023)


  expect_equal(colnames(x$seasons), cols_seasons)
  expect_equal(colnames(x$divisions), cols_divisions)
  expect_equal(colnames(x$teams), cols_teams)
  expect_equal(colnames(x$officials), cols_officials)
  expect_equal(colnames(x$brackets), cols_brackets)

  expect_s3_class(x$seasons, "data.frame")
  expect_s3_class(x$divisions, "data.frame")
  expect_s3_class(x$teams, "data.frame")
  expect_s3_class(x$officials, "data.frame")
  expect_s3_class(x$brackets, "data.frame")
})
