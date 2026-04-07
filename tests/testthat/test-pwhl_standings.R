test_that("PWHL - Get PWHL Standings", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_standings(season = 2023, regular = TRUE)

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)

    expected_cols <- c(
        "team_rank",
        "team",
        "team_code",
        "games_played",
        "points",
        "wins",
        "losses",
        "goals_for",
        "goals_against"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})
