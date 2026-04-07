test_that("phf_league_info is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    phf_league_info(season = 2023),
    class = "lifecycle_error_deprecated"
  )
})
