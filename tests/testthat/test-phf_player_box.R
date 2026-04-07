test_that("phf_player_box is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    phf_player_box(game_id = 40863),
    class = "lifecycle_error_deprecated"
  )
})
