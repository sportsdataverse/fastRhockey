test_that("PWHL - Get PWHL Team Roster", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_team_roster(team = "Toronto", season = 2024)

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)

    expected_cols <- c(
        "player_id",
        "first_name",
        "last_name",
        "position"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})
