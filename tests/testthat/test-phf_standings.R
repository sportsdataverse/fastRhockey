test_that("phf_standings is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    phf_standings(season = 2023),
    class = "lifecycle_error_deprecated"
  )
})
