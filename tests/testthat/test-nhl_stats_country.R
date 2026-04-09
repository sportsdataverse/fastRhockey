test_that("NHL - Stats Country", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_country()
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
    }
})
