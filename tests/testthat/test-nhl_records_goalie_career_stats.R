test_that("NHL Records - Goalie Career Stats", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_records_goalie_career_stats(limit = 5)
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
    }
})
