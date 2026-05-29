test_that("PWHL - Get PWHL Schedule", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_schedule(season = 2024)

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)

    expected_cols <- c(
        "game_id",
        "season",
        "game_date",
        "game_status",
        "home_team",
        "away_team",
        "home_score",
        "away_score",
        "venue",
        "game_type"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }

    # Default game_type = "both" must combine regular season + playoffs.
    expect_true(all(x$game_type %in% c("regular", "playoffs")))
    expect_true("regular" %in% x$game_type)
    expect_true("playoffs" %in% x$game_type)
})

test_that("PWHL - Schedule honors a single game_type", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_schedule(season = 2024, game_type = "regular")

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
    expect_true(all(x$game_type == "regular"))
})

test_that("PWHL - Schedule rejects an invalid game_type", {
    expect_error(pwhl_schedule(season = 2024, game_type = "bogus"))
})
