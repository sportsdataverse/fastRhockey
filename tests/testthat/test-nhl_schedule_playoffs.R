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
        gameDate = c("2024-04-22", "2024-04-24"),
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
        gameDate = c("2024-04-22", "2024-04-24", "2024-04-26"),
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
