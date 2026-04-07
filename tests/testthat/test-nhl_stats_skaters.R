test_that("NHL Stats - Get Skater Stats", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_skaters(season = "20242025")

    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
    expect_true("games_played" %in% names(x))
})

test_that("NHL Stats - Get Skater Bios", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_skaters(report = "bios", season = "20242025")

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
    expect_true("birth_date" %in% names(x))
})
