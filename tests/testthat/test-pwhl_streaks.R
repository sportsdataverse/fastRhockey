test_that("PWHL - Get PWHL Player Streaks", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_streaks(season = 2025)

    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)

        expect_true("game_type" %in% names(x))
        # Default game_type = "both": rows are labeled regular/playoffs only.
        expect_true(all(x$game_type %in% c("regular", "playoffs")))
    }
})

test_that("PWHL - Streaks honor a single game_type", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_streaks(season = 2024, game_type = "regular")

    if (!is.null(x)) {
        expect_true(all(x$game_type == "regular"))
    }
})

test_that("PWHL - Streaks reject an invalid game_type", {
    expect_error(pwhl_streaks(season = 2024, game_type = "bogus"))
})
