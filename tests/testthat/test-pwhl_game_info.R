test_that("PWHL - Get PWHL Game Info", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_game_info(game_id = 27)

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)

    expected_cols <- c(
        "game_id",
        "game_season",
        "game_type",
        "game_date",
        "home_team",
        "away_team",
        "home_team_id",
        "away_team_id",
        "home_score",
        "away_score",
        "game_duration",
        "game_venue",
        "game_season_id"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})
