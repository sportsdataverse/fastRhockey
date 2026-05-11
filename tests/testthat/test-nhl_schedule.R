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

test_that("NHL - nhl_schedule(season=, game_type='both') returns regular + playoff rows", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_schedule(season = 2024, team_abbr = "TOR", game_type = "both")

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
    expect_true("R" %in% x$game_type)
    expect_true("P" %in% x$game_type)

    # New context columns exist and are populated only on playoff rows.
    for (col in c("series_letter", "playoff_round", "series_game_number")) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
    playoff_rows <- x[x$game_type == "P", ]
    expect_true(all(!is.na(playoff_rows$series_letter)))
    expect_true(all(!is.na(playoff_rows$playoff_round)))
    expect_true(all(!is.na(playoff_rows$series_game_number)))

    regular_rows <- x[x$game_type == "R", ]
    expect_true(all(is.na(regular_rows$series_letter)))
    expect_true(all(is.na(regular_rows$playoff_round)))
    expect_true(all(is.na(regular_rows$series_game_number)))
})

test_that("NHL - nhl_schedule(season=, game_type='playoffs') returns only playoff rows", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_schedule(season = 2024, team_abbr = "TOR", game_type = "playoffs")

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
    expect_true(all(x$game_type == "P"))
    expect_true(all(!is.na(x$series_letter)))
})

test_that("NHL - nhl_schedule(season=, game_type='regular') returns only regular rows", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_schedule(season = 2024, team_abbr = "TOR", game_type = "regular")

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
    expect_true(all(x$game_type == "R"))
    # Context columns exist but are all NA in regular-only mode.
    expect_true(all(is.na(x$series_letter)))
    expect_true(all(is.na(x$playoff_round)))
    expect_true(all(is.na(x$series_game_number)))
})

test_that("NHL - nhl_schedule(day=) ignores game_type", {
    skip_on_cran()
    skip_nhl_test()
    x_default <- nhl_schedule(day = "2024-04-22")
    x_regular <- nhl_schedule(day = "2024-04-22", game_type = "regular")
    expect_equal(nrow(x_default), nrow(x_regular))
    # Compare the load-bearing columns directly so the assertion ignores
    # the fastRhockey_data timestamp attribute (which differs by a few
    # millis between the two calls).
    expect_identical(x_default$game_id, x_regular$game_id)
    expect_identical(x_default$game_type, x_regular$game_type)
})
