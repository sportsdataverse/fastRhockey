## Live test (network; skipped on CRAN and when RUN_PWHL_TESTS != "true") -------

test_that("PWHL - pwhl_player_toi returns expected columns", {
  skip_on_cran()
  skip_pwhl_test()

  x <- pwhl_player_toi(game_id = 42)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from pwhl_player_toi at test time")
  }

  expected_cols <- c(
    "player_id", "first_name", "last_name",
    "toi_seconds", "num_shifts", "avg_shift_s"
  )
  expect_true(
    all(expected_cols %in% names(x)),
    label = "all expected TOI columns present"
  )
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0, label = "at least one player row")
})
