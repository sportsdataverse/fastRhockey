test_that("NHL - Get Game Story", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_game_story(game_id = 2024020001)

    expect_type(x, "list")
    expect_true("id" %in% names(x))
    expect_true("season" %in% names(x))
    expect_true("gameType" %in% names(x))
})
