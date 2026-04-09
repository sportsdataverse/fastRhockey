test_that("NHL - Get Postal Lookup", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_postal_lookup(postal_code = "10001")

    if (!is.null(x)) {
        expect_type(x, "list")
    }
})
