test_that("NHL Records - Combine", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_records_combine()
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
    }
})
