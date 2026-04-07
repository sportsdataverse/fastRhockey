test_that("load_phf_pbp is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    load_phf_pbp(seasons = 2023),
    class = "lifecycle_error_deprecated"
  )
})

test_that("load_phf_team_box is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    load_phf_team_box(seasons = 2023),
    class = "lifecycle_error_deprecated"
  )
})

test_that("load_phf_player_box is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    load_phf_player_box(seasons = 2023),
    class = "lifecycle_error_deprecated"
  )
})

test_that("load_phf_schedule is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    load_phf_schedule(seasons = 2023),
    class = "lifecycle_error_deprecated"
  )
})

test_that("load_phf_rosters is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    load_phf_rosters(seasons = 2023),
    class = "lifecycle_error_deprecated"
  )
})

test_that("update_phf_db is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    update_phf_db(),
    class = "lifecycle_error_deprecated"
  )
})

test_that("most_recent_phf_season is deprecated", {
  skip_on_cran()
  skip_phf_test()
  expect_error(
    most_recent_phf_season(),
    class = "lifecycle_error_deprecated"
  )
})
