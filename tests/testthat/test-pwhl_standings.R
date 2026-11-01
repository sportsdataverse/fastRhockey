test_that("PWHL - Get PWHL Standings", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_standings(season = 2024, regular = TRUE)

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

    # The inaugural 2024 regular season: 6 teams, 24 games each.
    expect_equal(nrow(x), 6)
    expect_true(all(x$games_played == "24"))
})

test_that("PWHL - pwhl_standings() returns the season asked for", {
    skip_on_cran()
    skip_pwhl_test()
    # Before 1.1.0 every call sent HockeyTech season 1 and returned 2024.
    # 2025-26 added Seattle and Vancouver (8 teams) and plays 30 games.
    x <- pwhl_standings(season = 2026, regular = TRUE)

    expect_equal(nrow(x), 8)
    expect_true(all(x$games_played == "30"))
    expect_true(any(grepl("SEA", x$team_code)))
    expect_true(any(grepl("VAN", x$team_code)))
})
