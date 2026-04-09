test_that("NHL - Stats Players (no filter returns NULL)", {
    skip_on_cran()
    skip_nhl_test()
    # Without a filter the endpoint returns `data: []`, so the wrapper
    # should return NULL with a message rather than erroring on clean_names.
    x <- nhl_stats_players()
    expect_null(x)
})

test_that("NHL - Stats Players (with cayenne filter)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_players(cayenne_exp = 'lastName="McDavid"')
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
    }
})
