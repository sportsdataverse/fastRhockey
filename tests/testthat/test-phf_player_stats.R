test_that("phf_player_stats is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    phf_player_stats(player_id = 431611),
    class = "lifecycle_error_deprecated"
  )
})
