test_that("NHL Stats - Get Team Stats", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_teams(season = "20242025")

    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
    expect_true("games_played" %in% names(x))
})

test_that("NHL Stats - Get Team Penalty Stats", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_teams(report = "penalties", season = "20242025")

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
})
