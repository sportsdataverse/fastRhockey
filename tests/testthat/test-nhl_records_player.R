test_that("NHL Records - Player", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_records_player()
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
    }
})
