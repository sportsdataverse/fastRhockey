test_that("NHL - Get Smart Links", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_smartlinks()

    if (!is.null(x)) {
        expect_type(x, "list")
    }
})
