test_that("NHL - Get NHL Player Stats", {
  skip_on_cran()
  skip_nhl_test()
  x <- nhl_player_stats(player_id = 8476899)

  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  expect_true("player_id" %in% names(x))
  expect_true("first_name" %in% names(x))
  expect_true("last_name" %in% names(x))
  expect_true("position" %in% names(x))
  expect_true("games_played" %in% names(x))
  expect_true("season" %in% names(x))
})
