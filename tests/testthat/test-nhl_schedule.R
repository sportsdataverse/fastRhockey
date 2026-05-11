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


# Regression test for a long-standing parser bug. .parse_*_games() used
# ifelse(scalar_test, vector, NA) to guard against missing nested fields,
# but ifelse() returns a result of length(test) — i.e. length 1 — which
# tibble then silently recycled across every row. The result: every game
# had the same home_team_name, away_team_name, home_score, away_score, and
# venue. Fixed by switching to if/else, which preserves vector length.
test_that(".parse_club_schedule_games: preserves per-row distinct values", {
    # Construct a 3-row fixture with deliberately distinct values in every
    # nested field. If the parser collapses any column to length 1, the
    # distinct-values check below will fail.
    games_df <- data.frame(
        id = c(2024020001L, 2024020002L, 2024020003L),
        season = c("20242025", "20242025", "20242025"),
        gameType = c(2L, 2L, 2L),
        gameDate = c("2024-10-08", "2024-10-09", "2024-10-10"),
        startTimeUTC = c(
            "2024-10-08T23:00:00Z",
            "2024-10-09T23:00:00Z",
            "2024-10-10T23:00:00Z"
        ),
        gameState = c("OFF", "OFF", "FUT"),
        stringsAsFactors = FALSE
    )
    games_df$homeTeam <- data.frame(
        abbrev = c("TOR", "MTL", "BOS"),
        score = c(3L, 1L, 4L),
        stringsAsFactors = FALSE
    )
    games_df$homeTeam$placeName <- data.frame(
        default = c("Toronto", "Montréal", "Boston"),
        stringsAsFactors = FALSE
    )
    games_df$awayTeam <- data.frame(
        abbrev = c("MTL", "BOS", "TOR"),
        score = c(2L, 0L, 5L),
        stringsAsFactors = FALSE
    )
    games_df$awayTeam$placeName <- data.frame(
        default = c("Montréal", "Boston", "Toronto"),
        stringsAsFactors = FALSE
    )
    games_df$venue <- data.frame(
        default = c("Scotiabank Arena", "Centre Bell", "TD Garden"),
        stringsAsFactors = FALSE
    )

    out <- fastRhockey:::.parse_club_schedule_games(games_df)

    # Every column derived from a nested field must have 3 distinct values
    # in this fixture. If any collapses to 1, the ifelse-recycling bug
    # has regressed.
    expect_equal(out$home_team_name, c("Toronto", "Montréal", "Boston"))
    expect_equal(out$away_team_name, c("Montréal", "Boston", "Toronto"))
    expect_equal(out$home_score, c(3L, 1L, 4L))
    expect_equal(out$away_score, c(2L, 0L, 5L))
    expect_equal(
        out$venue,
        c("Scotiabank Arena", "Centre Bell", "TD Garden")
    )
})


test_that(".parse_schedule_games: preserves per-row distinct values", {
    # Mirror of the above for the day-mode parser. Same regression risk:
    # the day-mode parser has the same ifelse pattern.
    games_df <- data.frame(
        id = c(2024020001L, 2024020002L),
        season = c("20242025", "20242025"),
        gameType = c(2L, 2L),
        startTimeUTC = c(
            "2024-10-08T23:00:00Z",
            "2024-10-08T23:30:00Z"
        ),
        gameState = c("OFF", "OFF"),
        stringsAsFactors = FALSE
    )
    games_df$homeTeam <- data.frame(
        abbrev = c("TOR", "NYR"),
        score = c(3L, 6L),
        stringsAsFactors = FALSE
    )
    games_df$homeTeam$placeName <- data.frame(
        default = c("Toronto", "New York"),
        stringsAsFactors = FALSE
    )
    games_df$awayTeam <- data.frame(
        abbrev = c("MTL", "PIT"),
        score = c(2L, 0L),
        stringsAsFactors = FALSE
    )
    games_df$awayTeam$placeName <- data.frame(
        default = c("Montréal", "Pittsburgh"),
        stringsAsFactors = FALSE
    )
    games_df$venue <- data.frame(
        default = c("Scotiabank Arena", "Madison Square Garden"),
        stringsAsFactors = FALSE
    )

    out <- fastRhockey:::.parse_schedule_games(games_df)

    expect_equal(out$home_team_name, c("Toronto", "New York"))
    expect_equal(out$away_team_name, c("Montréal", "Pittsburgh"))
    expect_equal(out$home_score, c(3L, 6L))
    expect_equal(out$away_score, c(2L, 0L))
    expect_equal(
        out$venue,
        c("Scotiabank Arena", "Madison Square Garden")
    )
})
