test_that("NHL Records - Skater Real-Time Stats Season", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_records_skater_real_time_stats_season(limit = 5)
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
    }
})
