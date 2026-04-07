test_that("phf_team_roster is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    phf_team_roster(team = "Minnesota Whitecaps", season = 2023),
    class = "lifecycle_error_deprecated"
  )
})
