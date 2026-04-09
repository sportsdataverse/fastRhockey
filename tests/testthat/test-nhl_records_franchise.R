test_that("NHL Records - Franchise", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_records_franchise()
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
    }
})

test_that("NHL Records - Franchise (by id)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_records_franchise(franchise_id = 5)
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
    }
})
