test_that("NHL - Get NHL Teams Stats", {
  skip_on_cran()
  skip_nhl_test()
  x <- nhl_teams_stats(team_abbr = "TBL")

  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  expect_true("player_type" %in% names(x))
  expect_true("team_abbr" %in% names(x))
  expect_true("games_played" %in% names(x))
  expect_true(all(x$team_abbr == "TBL"))
  # Check both skaters and goalies are included
  expect_true("skater" %in% x$player_type)
  expect_true("goalie" %in% x$player_type)
})
