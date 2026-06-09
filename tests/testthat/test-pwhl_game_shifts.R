## Offline parse test (no network; uses fixture bundled with the package) --------

test_that("pwhl game shifts offline parse", {
  payload <- jsonlite::read_json(
    testthat::test_path("fixtures", "hockeytech", "pwhl_gameshifts_42.json")
  )
  df  <- fastRhockey:::.parse_hockeytech_shifts(payload, game_id = 42)
  toi <- fastRhockey:::hockeytech_player_toi(df)

  expect_true(
    all(c("toi_seconds", "num_shifts") %in% names(toi)) && nrow(toi) > 0,
    label = "toi has required columns and rows"
  )
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0, label = "shifts data frame is non-empty")

  expected_shift_cols <- c(
    "game_id", "player_id", "first_name", "last_name",
    "jersey_number", "home", "period",
    "start_time", "end_time", "length",
    "start_s", "end_s", "goal_on_shift", "penalty_on_shift"
  )
  expect_true(
    all(expected_shift_cols %in% names(df)),
    label = "all expected shift columns present"
  )
})


## Live test (network; skipped on CRAN and when RUN_PWHL_TESTS != "true") -------

test_that("PWHL - pwhl_game_shifts returns expected columns", {
  skip_on_cran()
  skip_pwhl_test()

  x <- pwhl_game_shifts(game_id = 42)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from pwhl_game_shifts at test time")
  }

  expected_cols <- c(
    "game_id", "player_id", "first_name", "last_name",
    "jersey_number", "home", "period",
    "start_time", "end_time", "length",
    "start_s", "end_s", "goal_on_shift", "penalty_on_shift"
  )
  expect_true(
    all(expected_cols %in% names(x)),
    label = "all expected columns present in live result"
  )
  expect_s3_class(x, "data.frame")
})
