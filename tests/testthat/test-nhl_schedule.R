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
    x <- nhl_schedule(team_abbr = "TOR", season = 2024)

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
