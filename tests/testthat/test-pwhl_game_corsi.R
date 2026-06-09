## Live test (network; skipped on CRAN and when RUN_PWHL_TESTS != "true") -------

test_that("PWHL - pwhl_game_corsi returns expected columns", {
  skip_on_cran()
  skip_pwhl_test()

  x <- pwhl_game_corsi(game_id = 42)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from pwhl_game_corsi at test time")
  }

  expected_cols <- c(
    "player_id",
    "corsi_for", "corsi_against", "corsi_for_pct",
    "fenwick_for", "fenwick_against", "fenwick_for_pct",
    "corsi_includes_missed",
    "toi_seconds", "corsi_for_per60"
  )
  expect_true(
    all(expected_cols %in% names(x)),
    label = "all expected Corsi columns present"
  )
  expect_s3_class(x, "data.frame")
  expect_s3_class(x, "fastRhockey_data")
  expect_true(nrow(x) > 0, label = "at least one on-ice player row")
  expect_false(
    any(x$corsi_includes_missed, na.rm = TRUE),
    label = "corsi_includes_missed is always FALSE"
  )
})
