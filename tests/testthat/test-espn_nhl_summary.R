test_that("ESPN - Get ESPN NHL Raw Summary", {
  skip_on_cran()
  skip_espn_test()

  sb  <- espn_nhl_scoreboard(dates = "20250110")
  if (!is.data.frame(sb) || nrow(sb) == 0) {
    skip("No scoreboard rows returned at test time")
  }
  gid <- sb$game_id[1]

  s <- espn_nhl_summary(game_id = gid)

  if (length(s) == 0) {
    skip("No data returned from ESPN NHL summary endpoint at test time")
  }

  expect_type(s, "list")
  expect_true(length(s) > 0)

  # Core top-level keys should be present
  expect_true("boxscore" %in% names(s))
  expect_true("header"   %in% names(s))
  expect_true("plays"    %in% names(s))

  # plays should be a non-empty list
  expect_true(length(s$plays) > 0)

  # boxscore should have teams and players
  expect_true("teams"   %in% names(s$boxscore))
  expect_true("players" %in% names(s$boxscore))

  Sys.sleep(1)
})
