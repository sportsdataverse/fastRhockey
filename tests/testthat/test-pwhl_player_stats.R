test_that("PWHL - Get PWHL Player Season Stats", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_player_stats(player_id = 28)

    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)

        expected_cols <- c(
            "player_id",
            "season_name",
            "games_played",
            "goals",
            "assists",
            "points"
        )
        for (col in expected_cols) {
            expect_true(col %in% names(x), info = paste("Missing column:", col))
        }

        expect_true(all(x$player_id == 28))
    }
})
