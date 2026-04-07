test_that("NHL - Get Scores (now)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_scores()

    # May be NULL if no games today
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("id" %in% names(x))
    }
})

test_that("NHL - Get Scores (specific date)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_scores(date = "2025-01-15")

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
    expect_true("game_date" %in% names(x))
})
