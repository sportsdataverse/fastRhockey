test_that("NHL - Get Game PBP (v1 wrapper)", {
    skip_on_cran()
    skip_nhl_test()
    x <- suppressWarnings(nhl_game_pbp(game_id = 2023020001))

    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_true(nrow(x) > 0)
    }
})
