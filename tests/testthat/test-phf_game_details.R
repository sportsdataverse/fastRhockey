test_that("phf_game_details is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    phf_game_details(game_id = 612254),
    class = "lifecycle_error_deprecated"
  )
})
