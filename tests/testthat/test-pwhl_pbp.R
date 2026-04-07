test_that("PWHL - Get PWHL Play-by-Play", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_pbp(game_id = 27)

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)

    expected_cols <- c(
        "game_id",
        "event",
        "period_of_game",
        "time_of_period",
        "team_id"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})
