test_that("load_nhl_pbp returns data for a season", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- load_nhl_pbp(seasons = 2021)
    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
})

test_that("load_nhl_schedule returns data for a season", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- load_nhl_schedule(seasons = 2021)
    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
})

test_that("load_nhl_team_box returns data for a season", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- load_nhl_team_box(seasons = 2021)
    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
})

test_that("load_nhl_player_box returns data for a season", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- load_nhl_player_box(seasons = 2021)
    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
})

test_that("load_nhl_rosters returns data for a season", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- load_nhl_rosters(seasons = 2021)
    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
})
