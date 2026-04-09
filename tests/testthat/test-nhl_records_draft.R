test_that("NHL Records - Draft", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_records_draft(
        cayenne_exp = "draftYear=2020",
        limit = 5
    )
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
    }
})
