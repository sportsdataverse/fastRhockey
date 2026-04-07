test_that("NHL - Where to Watch", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_where_to_watch()

    # Endpoint may return NULL if no broadcasts scheduled; just verify no error
    expect_true(is.list(x) || is.null(x))
})
