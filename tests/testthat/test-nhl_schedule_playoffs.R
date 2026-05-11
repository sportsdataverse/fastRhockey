test_that(".extract_series_map: handles empty/NULL carousel", {
    expect_null(fastRhockey:::.extract_series_map(NULL))
    expect_null(fastRhockey:::.extract_series_map(list()))
    expect_null(fastRhockey:::.extract_series_map(list(rounds = NULL)))
})

test_that(".extract_series_map: extracts letter and round from nested rounds", {
    # Carousel structure produced by jsonlite::fromJSON(..., flatten = TRUE):
    # $rounds is a data frame; each row has $series (a list-column of data frames).
    fake_carousel <- list(
        rounds = data.frame(
            roundNumber = c(1L, 2L),
            stringsAsFactors = FALSE
        )
    )
    fake_carousel$rounds$series <- list(
        data.frame(
            seriesLetter = c("a", "b", "c", "d"),
            stringsAsFactors = FALSE
        ),
        data.frame(
            seriesLetter = c("i", "j"),
            stringsAsFactors = FALSE
        )
    )

    out <- fastRhockey:::.extract_series_map(fake_carousel)

    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 6L)
    expect_setequal(out$series_letter, c("a", "b", "c", "d", "i", "j"))
    expect_equal(
        out$playoff_round[out$series_letter == "a"], 1L
    )
    expect_equal(
        out$playoff_round[out$series_letter == "i"], 2L
    )
})

test_that(".extract_series_map: tolerates letterCode alternate field name", {
    fake_carousel <- list(
        rounds = data.frame(
            roundNumber = 1L,
            stringsAsFactors = FALSE
        )
    )
    fake_carousel$rounds$series <- list(
        data.frame(
            letterCode = c("a", "b"),
            stringsAsFactors = FALSE
        )
    )
    out <- fastRhockey:::.extract_series_map(fake_carousel)
    expect_equal(nrow(out), 2L)
    expect_setequal(out$series_letter, c("a", "b"))
})

test_that(".parse_playoff_series_games: returns 16-column tibble with context populated", {
    # Construct minimal games_df with the nested structure that
    # jsonlite::fromJSON(..., flatten = FALSE) produces.
    games_df <- data.frame(
        id = c(2023030111L, 2023030112L),
        season = c("20232024", "20232024"),
        gameType = c(3L, 3L),
        startTimeUTC = c("2024-04-23T00:00:00Z", "2024-04-24T23:00:00Z"),
        gameState = c("OFF", "OFF"),
        stringsAsFactors = FALSE
    )
    games_df$homeTeam <- data.frame(
        abbrev = c("TOR", "BOS"),
        score = c(3L, 5L),
        stringsAsFactors = FALSE
    )
    games_df$homeTeam$placeName <- data.frame(
        default = c("Toronto", "Boston"),
        stringsAsFactors = FALSE
    )
    games_df$awayTeam <- data.frame(
        abbrev = c("BOS", "TOR"),
        score = c(1L, 4L),
        stringsAsFactors = FALSE
    )
    games_df$awayTeam$placeName <- data.frame(
        default = c("Boston", "Toronto"),
        stringsAsFactors = FALSE
    )
    games_df$venue <- data.frame(
        default = c("Scotiabank Arena", "TD Garden"),
        stringsAsFactors = FALSE
    )

    out <- fastRhockey:::.parse_playoff_series_games(
        games_df,
        series_letter = "a",
        playoff_round = 1L
    )

    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 2L)

    expected_cols <- c(
        "game_id", "season_full", "game_type", "game_date", "game_time",
        "home_team_abbr", "away_team_abbr", "home_team_name", "away_team_name",
        "home_score", "away_score", "game_state", "venue",
        "series_letter", "playoff_round", "series_game_number"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(out), info = paste("Missing column:", col))
    }
    expect_setequal(names(out), expected_cols)

    expect_equal(unique(out$series_letter), "a")
    expect_equal(unique(out$playoff_round), 1L)
    expect_equal(out$series_game_number, c(1L, 2L))
    expect_equal(unique(out$game_type), "P")
})

test_that(".parse_playoff_series_games: uses gameNumber when present", {
    games_df <- data.frame(
        id = c(2023030111L, 2023030112L, 2023030113L),
        season = c("20232024", "20232024", "20232024"),
        gameType = c(3L, 3L, 3L),
        startTimeUTC = c(
            "2024-04-23T00:00:00Z",
            "2024-04-24T23:00:00Z",
            "2024-04-27T00:00:00Z"
        ),
        gameState = c("OFF", "OFF", "OFF"),
        gameNumber = c(7L, 5L, 6L),  # deliberately out of chrono order
        stringsAsFactors = FALSE
    )
    games_df$homeTeam <- data.frame(
        abbrev = c("TOR", "BOS", "TOR"),
        score = c(3L, 5L, 2L),
        stringsAsFactors = FALSE
    )
    games_df$homeTeam$placeName <- data.frame(
        default = c("Toronto", "Boston", "Toronto"),
        stringsAsFactors = FALSE
    )
    games_df$awayTeam <- data.frame(
        abbrev = c("BOS", "TOR", "BOS"),
        score = c(1L, 4L, 3L),
        stringsAsFactors = FALSE
    )
    games_df$awayTeam$placeName <- data.frame(
        default = c("Boston", "Toronto", "Boston"),
        stringsAsFactors = FALSE
    )
    games_df$venue <- data.frame(
        default = c("Scotiabank Arena", "TD Garden", "Scotiabank Arena"),
        stringsAsFactors = FALSE
    )

    out <- fastRhockey:::.parse_playoff_series_games(
        games_df,
        series_letter = "b",
        playoff_round = 1L
    )

    # Should use the API-provided gameNumber, not chronological order.
    expect_equal(out$series_game_number, c(7L, 5L, 6L))
})

test_that(".fetch_nhl_season_playoffs: returns playoff games for 2023-24", {
    skip_on_cran()
    skip_nhl_test()

    # season = 2024 means the 2023-24 season (end-year convention).
    out <- fastRhockey:::.fetch_nhl_season_playoffs(season = 2024)

    expect_s3_class(out, "data.frame")
    expect_true(nrow(out) > 0)
    expect_true(all(out$game_type == "P"))
    expect_true(all(!is.na(out$series_letter)))
    expect_true(all(!is.na(out$playoff_round)))
    expect_true(all(!is.na(out$series_game_number)))
    expect_true(all(out$playoff_round %in% 1L:4L))
})

test_that(".fetch_nhl_season_playoffs: returns empty tibble with the 16-col schema when carousel is NULL", {
    local_mocked_bindings(
        nhl_playoff_carousel = function(...) NULL,
        .package = "fastRhockey"
    )

    out <- suppressMessages(
        fastRhockey:::.fetch_nhl_season_playoffs(season = 2024)
    )

    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 0L)
    expected_cols <- c(
        "game_id", "season_full", "game_type", "game_date", "game_time",
        "home_team_abbr", "away_team_abbr", "home_team_name", "away_team_name",
        "home_score", "away_score", "game_state", "venue",
        "series_letter", "playoff_round", "series_game_number"
    )
    expect_setequal(names(out), expected_cols)
})

test_that(".fetch_nhl_season_playoffs: returns empty tibble when no playoffs", {
    skip_on_cran()
    skip_nhl_test()

    # Use a far-future season that cannot yet have playoff data.
    out <- suppressMessages(
        fastRhockey:::.fetch_nhl_season_playoffs(season = 2099)
    )

    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 0L)
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
