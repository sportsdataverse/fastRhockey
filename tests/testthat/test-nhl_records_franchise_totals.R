test_that("NHL Records - Franchise Totals", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_records_franchise_totals()
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
    }
})
