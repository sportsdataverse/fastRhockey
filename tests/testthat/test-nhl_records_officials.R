test_that("NHL Records - Officials", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_records_officials()
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
    }
})
