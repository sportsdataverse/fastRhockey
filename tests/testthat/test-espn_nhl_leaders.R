test_that("ESPN NHL Leaders returns valid wide tibble", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_leaders(category = "offensive", limit = 25, page = 1)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_leaders at test time")
  }

  key_cols <- c(
    "season",
    "season_type",
    "requested_season_year",
    "pagination_count",
    "pagination_limit",
    "pagination_pages",
    "league_id",
    "league_name",
    "athlete_id",
    "athlete_display_name",
    "athlete_position",
    "athlete_team_id",
    "athlete_team_name"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) <= 25L)
  # At least one offensive stat column present
  expect_true(any(grepl("^offensive_", colnames(x))))

  Sys.sleep(1)
})
