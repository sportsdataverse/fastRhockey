test_that("phf_game_details", {
  skip_on_cran()
  cols <- c(
    "game_id",
    "game_date",
    "home_team",
    "home_location",
    "home_nickname",
    "home_abbreviation",
    "home_score_total",
    "away_team",
    "away_location",
    "away_nickname",
    "away_abbreviation",
    "away_score_total"
  )
  x <- phf_game_details(game_id = 612254)

  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")

})
