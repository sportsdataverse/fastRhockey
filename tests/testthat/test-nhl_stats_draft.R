test_that("NHL - Get Stats Draft data", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_draft()

    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
    }
})

test_that("NHL - Get Stats Draft by year", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_draft(draft_year = 2024)

    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_true(nrow(x) > 0)
    }
})
