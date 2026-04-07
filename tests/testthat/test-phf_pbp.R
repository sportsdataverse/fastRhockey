test_that("phf_pbp is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    phf_pbp(game_id = 40863),
    class = "lifecycle_error_deprecated"
  )
})
