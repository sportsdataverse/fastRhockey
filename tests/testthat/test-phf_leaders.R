test_that("phf_leaders is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    phf_leaders(player_type = "skaters", season = 2022, season_type = "Regular Season"),
    class = "lifecycle_error_deprecated"
  )
})
