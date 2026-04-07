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
        "venue"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})
