test_that("NHL - Stats Config", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_config()
    if (!is.null(x)) {
        expect_type(x, "list")
    }
})
