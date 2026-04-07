test_that("NHL - Get Skater Stats Leaders (current)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_skater_stats_leaders()

    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
    expect_true("category" %in% names(x))
})

test_that("NHL - Get Skater Stats Leaders (specific season)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_skater_stats_leaders(season = 2023, game_type = 2)

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
})
