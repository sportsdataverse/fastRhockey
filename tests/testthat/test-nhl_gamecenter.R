test_that("NHL - Get Gamecenter Landing", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_gamecenter_landing(game_id = 2024020001)

    expect_type(x, "list")
    expect_true("id" %in% names(x))
    expect_true("season" %in% names(x))
})

test_that("NHL - Get Gamecenter Right Rail", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_gamecenter_right_rail(game_id = 2024020001)

    expect_type(x, "list")
    expect_true("seasonSeries" %in% names(x) || "gameInfo" %in% names(x))
})
