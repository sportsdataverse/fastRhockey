test_that("NHL - Get Player Spotlight", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_player_spotlight()

    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
})
