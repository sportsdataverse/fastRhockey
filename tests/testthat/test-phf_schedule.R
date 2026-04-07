test_that("phf_schedule is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    phf_schedule(season = 2023),
    class = "lifecycle_error_deprecated"
  )
})
