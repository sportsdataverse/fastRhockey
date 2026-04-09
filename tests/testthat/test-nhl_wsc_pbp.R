test_that("NHL - Get WSC Play-by-Play", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_wsc_pbp(game_id = 2023020001)

    if (!is.null(x)) {
        expect_type(x, "list")
    }
})
