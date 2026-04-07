test_that("NHL - Get Standings Seasons", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_standings_season()

    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
    expect_true("id" %in% names(x))
    expect_true("conferences_in_use" %in% names(x))
})
