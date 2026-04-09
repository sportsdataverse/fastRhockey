test_that("NHL - Player Career Stats", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_player_career_stats(player_id = 8478402)
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
        expect_true("player_id" %in% names(x))
        expect_true("first_name" %in% names(x))
        expect_true("last_name" %in% names(x))
        expect_true("season" %in% names(x))
    }
})
