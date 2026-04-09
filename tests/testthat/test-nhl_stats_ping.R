test_that("NHL - Stats Ping", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_ping()
    if (!is.null(x)) {
        expect_type(x, "list")
    }
})
