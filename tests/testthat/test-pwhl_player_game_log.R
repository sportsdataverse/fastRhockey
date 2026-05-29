test_that("PWHL - Get PWHL Player Game Log", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_player_game_log(player_id = 28, season = 2025)

    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)

        expected_cols <- c(
            "player_id",
            "id",
            "date_played",
            "game_type"
        )
        for (col in expected_cols) {
            expect_true(col %in% names(x), info = paste("Missing column:", col))
        }

        expect_true(all(x$player_id == 28))
        # Default game_type = "both": rows are labeled regular/playoffs only.
        expect_true(all(x$game_type %in% c("regular", "playoffs")))
    }
})

test_that("PWHL - Player game log honors a single game_type", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_player_game_log(player_id = 28, season = 2024, game_type = "regular")

    if (!is.null(x)) {
        expect_true(all(x$game_type == "regular"))
    }
})

test_that("PWHL - Player game log rejects an invalid game_type", {
    expect_error(
        pwhl_player_game_log(player_id = 28, season = 2024, game_type = "bogus")
    )
})
