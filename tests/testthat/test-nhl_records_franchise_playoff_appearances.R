test_that("NHL Records - Franchise Playoff Appearances", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_records_franchise_playoff_appearances()
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
    }
})
