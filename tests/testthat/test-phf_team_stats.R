test_that("phf_team_stats is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    phf_team_stats(team = "Boston Pride", season = 2022),
    class = "lifecycle_error_deprecated"
  )
})
