test_that("NHL - Get Player Game Log (current season)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_player_game_log(player_id = 8478402)

    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
    expect_true("player_id" %in% names(x))
})

test_that("NHL - Get Player Game Log (specific season)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_player_game_log(player_id = 8478402, season = 2023, game_type = 2)

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
})

test_that("NHL - Get Player Game Log (playoffs)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_player_game_log(player_id = 8478402, season = 2023, game_type = 3)

    # May be NULL if no playoff data, or a data frame
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
    }
})
