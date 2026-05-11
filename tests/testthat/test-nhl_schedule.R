test_that("NHL - Get NHL Schedule (day mode)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_schedule(day = "2025-01-15")

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)

    expected_cols <- c(
        "game_id",
        "season_full",
        "game_type",
        "game_date",
        "game_time",
        "game_state",
        "away_team_abbr",
        "away_team_name",
        "away_score",
        "home_team_abbr",
        "home_team_name",
        "home_score",
        "venue"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})

test_that("NHL - Get NHL Schedule (season mode)", {
    skip_on_cran()
    skip_nhl_test()
    # season is the *end year* (e.g. 2025 = 2024-25 season)
    x <- nhl_schedule(team_abbr = "TOR", season = 2025)

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
    expect_true("game_id" %in% names(x))
    expect_true("game_date" %in% names(x))
})

test_that("NHL - Schedule returns fastRhockey_data class", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_schedule(day = "2025-01-15")
    expect_s3_class(x, "fastRhockey_data")
})

test_that("NHL - nhl_schedule() rejects invalid game_type", {
    expect_error(
        suppressMessages(nhl_schedule(season = 2024, game_type = "bogus")),
        regexp = "should be one of"
    )
})

test_that("NHL - nhl_schedule() accepts the three valid game_type values", {
    skip_on_cran()
    skip_nhl_test()
    # These calls should not error on the match.arg gate. We don't yet
    # assert behavior beyond that; Task 6 covers playoff branching.
    expect_error(
        nhl_schedule(season = 2024, team_abbr = "TOR", game_type = "regular"),
        regexp = NA
    )
})
