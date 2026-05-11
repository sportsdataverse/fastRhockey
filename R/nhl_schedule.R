#' @title **NHL Schedule**
#' @description Returns NHL schedule data for a given day or season.
#' Uses the NHL API (`api-web.nhle.com`).
#' @param day Character date in "YYYY-MM-DD" format. If provided, returns
#'   games for that specific day.
#' @param season Integer four-digit year for the *end year* of the season
#'   (e.g., 2026 for the 2025-26 season), matching
#'   [most_recent_nhl_season()]. If provided instead of `day`, returns
#'   the full season schedule.
#' @param team_abbr Character three-letter team abbreviation (e.g., "TOR").
#'   Required when `season` is used. If NULL, loops through all teams.
#' @param game_type Character, one of `"both"` (default), `"regular"`, or
#'   `"playoffs"`. Applies only in season mode; silently ignored when `day`
#'   is supplied. Default `"both"` returns regular-season and playoff games
#'   for the requested `season` in a single tibble.
#' @param include_data_flags Logical (default `FALSE`). When `TRUE`, after
#'   building the live schedule the result is left-joined against the
#'   pre-compiled `nhl_games_in_data_repo` index from
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   to add per-game data-availability flags
#'   (`PBP`, `team_box`, `player_box`, `skater_box`, `goalie_box`,
#'   `game_info`, `game_rosters`, `scoring`, `penalties`, `scratches`,
#'   `linescore`, `three_stars`, `shifts`, `officials`, `shots_by_period`,
#'   `shootout`). Games not yet compiled get `FALSE`. This requires a
#'   network call to the data repo and adds a small delay.
#' @return Returns a data frame with game schedule information. When
#'   `include_data_flags = TRUE` it additionally carries one logical column
#'   per pre-compiled dataset.
#' @keywords NHL Schedule
#' @importFrom jsonlite read_json fromJSON toJSON
#' @importFrom dplyr bind_rows mutate select rename filter distinct arrange
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom lubridate ymd
#' @export
#' @examples
#' \donttest{
#'   try(nhl_schedule(day = "2024-01-15"))
#'   try(nhl_schedule(season = 2025, team_abbr = "TOR"))
#'   try(nhl_schedule(season = 2024, team_abbr = "TOR", game_type = "playoffs"))
#'   try(nhl_schedule(day = "2024-01-15", include_data_flags = TRUE))
#' }
nhl_schedule <- function(day = NULL, season = NULL, team_abbr = NULL,
                         include_data_flags = FALSE,
                         game_type = c("both", "regular", "playoffs")) {
    game_type <- match.arg(game_type)

    if (is.null(day) && is.null(season)) {
        day <- as.character(Sys.Date())
    }

    # --- Single day schedule ---
    if (!is.null(day)) {
        url <- glue::glue("https://api-web.nhle.com/v1/schedule/{day}")
        tryCatch(
            expr = {
                raw <- jsonlite::read_json(url, simplifyVector = TRUE)
                game_week <- raw[["gameWeek"]]
                if (is.null(game_week) || length(game_week) == 0) {
                    message(glue::glue(
                        "{Sys.time()}: No games found for {day}"
                    ))
                    return(NULL)
                }
                games_list <- lapply(seq_len(nrow(game_week)), function(i) {
                    g <- game_week$games[[i]]
                    if (is.null(g) || nrow(g) == 0) {
                        return(NULL)
                    }
                    g$game_date_str <- game_week$date[i]
                    g
                })
                games_df <- dplyr::bind_rows(games_list)
                if (nrow(games_df) == 0) {
                    message(glue::glue(
                        "{Sys.time()}: No games found for {day}"
                    ))
                    return(NULL)
                }
                schedule <- .parse_schedule_games(games_df)
                if (isTRUE(include_data_flags)) {
                    schedule <- .nhl_attach_data_flags(schedule)
                }
                schedule <- make_fastRhockey_data(
                    schedule,
                    "NHL Schedule",
                    Sys.time()
                )
                return(schedule)
            },
            error = function(e) {
                message(glue::glue(
                    "{Sys.time()}: Error fetching schedule for {day}: {e$message}"
                ))
                return(NULL)
            }
        )
    }

    # --- Full season schedule ---
    if (!is.null(season)) {
        # `season` is the end year (e.g. 2026 = 2025-26)
        season_str <- paste0(season - 1, season)

        regular_df <- .empty_schedule_tibble()
        playoff_df <- .empty_schedule_tibble()

        # Regular-season half
        if (game_type %in% c("both", "regular")) {
            if (!is.null(team_abbr)) {
                teams <- team_abbr
            } else {
                teams <- fastRhockey::nhl_team_logos$team_abbr
                if (is.null(teams) || length(teams) == 0) {
                    teams <- c(
                        "ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY",
                        "CHI", "COL", "DAL", "DET", "EDM", "FLA", "LAK",
                        "MIN", "MTL", "NJD", "NSH", "NYI", "NYR", "OTT",
                        "PHI", "PIT", "SEA", "SJK", "STL", "TBL", "TOR",
                        "UTA", "VAN", "VGK", "WPG", "WSH"
                    )
                }
            }

            all_games <- list()
            for (tm in teams) {
                url <- glue::glue(
                    "https://api-web.nhle.com/v1/club-schedule-season/{tm}/{season_str}"
                )
                tryCatch(
                    expr = {
                        raw <- jsonlite::read_json(url, simplifyVector = TRUE)
                        games <- raw[["games"]]
                        if (!is.null(games) && nrow(games) > 0) {
                            all_games[[tm]] <- games
                        }
                    },
                    error = function(e) {
                        message(glue::glue(
                            "{Sys.time()}: Could not fetch schedule for {tm}: {e$message}"
                        ))
                    }
                )
            }

            if (length(all_games) > 0) {
                games_df <- dplyr::bind_rows(all_games)
                regular_df <- .parse_club_schedule_games(games_df)
                # club-schedule-season returns preseason (PR), regular (R)
                # and playoff (P) games. Keep only regular-season rows here;
                # playoff rows come from the playoff branch with full context
                # columns populated.
                regular_df <- dplyr::filter(
                    regular_df,
                    .data$game_type == "R"
                )
                regular_df$series_letter <- NA_character_
                regular_df$playoff_round <- NA_integer_
                regular_df$series_game_number <- NA_integer_
            }
        }

        # Playoff half
        if (game_type %in% c("both", "playoffs")) {
            playoff_df <- .fetch_nhl_season_playoffs(season)
            if (!is.null(team_abbr) && nrow(playoff_df) > 0) {
                playoff_df <- dplyr::filter(
                    playoff_df,
                    .data$home_team_abbr == team_abbr |
                        .data$away_team_abbr == team_abbr
                )
            }
        }

        if (nrow(regular_df) == 0 && nrow(playoff_df) == 0) {
            message(glue::glue(
                "{Sys.time()}: No games found for season {season_str}"
            ))
            return(NULL)
        }

        # Playoffs first so they win on game_id collisions (preserving the
        # populated context columns).
        schedule <- dplyr::bind_rows(playoff_df, regular_df) %>%
            dplyr::distinct(.data$game_id, .keep_all = TRUE) %>%
            dplyr::arrange(.data$game_date, .data$game_id)

        if (isTRUE(include_data_flags)) {
            schedule <- .nhl_attach_data_flags(schedule)
        }

        schedule <- make_fastRhockey_data(schedule, "NHL Schedule", Sys.time())
        return(schedule)
    }
}


# Internal: left-join the cached games_in_data_repo flags onto a live
# schedule tibble. Returns the input unchanged on any error so callers
# never silently lose data. Adds one logical column per pre-compiled
# dataset; games not yet compiled get FALSE.
.nhl_attach_data_flags <- function(schedule) {
    if (!is.data.frame(schedule) || nrow(schedule) == 0 ||
        !"game_id" %in% names(schedule)) {
        return(schedule)
    }

    flag_cols <- c(
        "PBP", "team_box", "player_box", "skater_box", "goalie_box",
        "game_info", "game_rosters", "scoring", "penalties", "scratches",
        "linescore", "three_stars", "shifts",
        "officials", "shots_by_period", "shootout"
    )

    games_idx <- tryCatch(load_nhl_games(), error = function(e) NULL)
    if (is.null(games_idx) || !"game_id" %in% names(games_idx)) {
        # Fall back: tag every game as FALSE so the columns exist
        for (fc in flag_cols) schedule[[fc]] <- FALSE
        return(schedule)
    }

    have_cols <- intersect(flag_cols, names(games_idx))
    join_df <- games_idx[, c("game_id", have_cols), drop = FALSE]

    out <- dplyr::left_join(
        schedule,
        join_df,
        by = "game_id"
    )
    # Coalesce any new flag columns to FALSE for non-matching rows
    for (fc in flag_cols) {
        if (fc %in% names(out)) {
            out[[fc]] <- dplyr::coalesce(as.logical(out[[fc]]), FALSE)
        } else {
            out[[fc]] <- FALSE
        }
    }
    out
}


#' Parse games from the daily schedule endpoint
#' @param games_df Data frame of games from the gameWeek structure
#' @return Parsed data frame
#' @keywords internal
.parse_schedule_games <- function(games_df) {
    schedule <- dplyr::tibble(
        game_id = games_df$id,
        season_full = as.character(games_df$season),
        game_type = dplyr::case_when(
            games_df$gameType == 1 ~ "PR",
            games_df$gameType == 2 ~ "R",
            games_df$gameType == 3 ~ "P",
            games_df$gameType == 4 ~ "A",
            TRUE ~ as.character(games_df$gameType)
        ),
        game_date = as.character(
            if ("game_date_str" %in% names(games_df)) {
                games_df$game_date_str
            } else {
                as.Date(games_df$startTimeUTC)
            }
        ),
        game_time = games_df$startTimeUTC,
        home_team_abbr = games_df$homeTeam$abbrev,
        away_team_abbr = games_df$awayTeam$abbrev,
        home_team_name = ifelse(
            !is.null(games_df$homeTeam$placeName$default),
            games_df$homeTeam$placeName$default,
            NA_character_
        ),
        away_team_name = ifelse(
            !is.null(games_df$awayTeam$placeName$default),
            games_df$awayTeam$placeName$default,
            NA_character_
        ),
        home_score = ifelse(
            !is.null(games_df$homeTeam$score),
            games_df$homeTeam$score,
            NA_integer_
        ),
        away_score = ifelse(
            !is.null(games_df$awayTeam$score),
            games_df$awayTeam$score,
            NA_integer_
        ),
        game_state = games_df$gameState,
        venue = ifelse(
            !is.null(games_df$venue$default),
            games_df$venue$default,
            NA_character_
        )
    )
    return(schedule)
}


#' Parse games from the club-schedule-season endpoint
#' @param games_df Data frame of games from the club schedule
#' @return Parsed data frame
#' @keywords internal
.parse_club_schedule_games <- function(games_df) {
    schedule <- dplyr::tibble(
        game_id = games_df$id,
        season_full = as.character(games_df$season),
        game_type = dplyr::case_when(
            games_df$gameType == 1 ~ "PR",
            games_df$gameType == 2 ~ "R",
            games_df$gameType == 3 ~ "P",
            games_df$gameType == 4 ~ "A",
            TRUE ~ as.character(games_df$gameType)
        ),
        game_date = as.character(games_df$gameDate),
        game_time = games_df$startTimeUTC,
        home_team_abbr = games_df$homeTeam$abbrev,
        away_team_abbr = games_df$awayTeam$abbrev,
        home_team_name = ifelse(
            !is.null(games_df$homeTeam$placeName$default),
            games_df$homeTeam$placeName$default,
            NA_character_
        ),
        away_team_name = ifelse(
            !is.null(games_df$awayTeam$placeName$default),
            games_df$awayTeam$placeName$default,
            NA_character_
        ),
        home_score = ifelse(
            !is.null(games_df$homeTeam$score),
            games_df$homeTeam$score,
            NA_integer_
        ),
        away_score = ifelse(
            !is.null(games_df$awayTeam$score),
            games_df$awayTeam$score,
            NA_integer_
        ),
        game_state = games_df$gameState,
        venue = ifelse(
            !is.null(games_df$venue$default),
            games_df$venue$default,
            NA_character_
        )
    )
    return(schedule)
}


#' Fetch raw playoff series schedule
#'
#' Internal HTTP helper for the `/v1/schedule/playoff-series/{season}/{letter}/`
#' endpoint. Used by `nhl_playoff_schedule()` and by the playoff branch of
#' `nhl_schedule()`.
#'
#' @param api_season Character 8-digit season string, e.g. `"20232024"`.
#' @param series_letter Character series letter, e.g. `"a"`.
#' @param flatten Logical, passed through to `jsonlite::fromJSON()`. Default
#'   `TRUE` preserves the existing return shape used by `nhl_playoff_schedule()`.
#'   Pass `FALSE` from the orchestrator so nested team objects are preserved
#'   for `.parse_club_schedule_games()`-style parsing.
#' @return Parsed list/data.frame from `jsonlite::fromJSON()`, or `NULL` on
#'   HTTP failure (a `message()` is emitted).
#' @keywords internal
#' @noRd
.fetch_playoff_series <- function(api_season, series_letter, flatten = TRUE) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/schedule/playoff-series/{api_season}/{series_letter}/"
    )

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)
            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = flatten)
            return(raw)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching playoff series {series_letter} for {api_season}: {e$message}"
            ))
            return(NULL)
        }
    )
}


#' Extract a series_letter / playoff_round map from a playoff carousel response
#'
#' @param carousel Parsed result from `nhl_playoff_carousel()` (with the
#'   default `flatten = TRUE` parse). May be `NULL`.
#' @return A data frame with columns `series_letter` (character) and
#'   `playoff_round` (integer), one row per playoff series. Returns `NULL`
#'   when the carousel is `NULL`, empty, or missing the expected structure.
#' @keywords internal
#' @noRd
.extract_series_map <- function(carousel) {
    if (is.null(carousel)) {
        return(NULL)
    }
    rounds <- carousel$rounds
    if (is.null(rounds) || NROW(rounds) == 0 || !"series" %in% names(rounds)) {
        return(NULL)
    }
    if (!"roundNumber" %in% names(rounds)) {
        return(NULL)
    }

    pieces <- lapply(seq_len(nrow(rounds)), function(i) {
        s <- rounds$series[[i]]
        if (is.null(s) || NROW(s) == 0) {
            return(NULL)
        }
        letters <- if ("seriesLetter" %in% names(s)) {
            s$seriesLetter
        } else if ("letterCode" %in% names(s)) {
            s$letterCode
        } else {
            return(NULL)
        }
        data.frame(
            series_letter = as.character(letters),
            playoff_round = as.integer(rounds$roundNumber[i]),
            stringsAsFactors = FALSE
        )
    })

    out <- do.call(rbind, pieces)
    if (is.null(out) || nrow(out) == 0) {
        return(NULL)
    }
    out
}

#' Parse games from a playoff-series schedule response into the schedule schema
#'
#' Reuses `.parse_club_schedule_games()` for the base 13 columns and appends
#' the three playoff context columns.
#'
#' @param games_df Data frame at `raw$games` from
#'   `.fetch_playoff_series(..., flatten = FALSE)`.
#' @param series_letter Character series letter for these games.
#' @param playoff_round Integer round number (from the carousel).
#' @return 16-column tibble matching the `nhl_schedule()` output schema.
#' @keywords internal
#' @noRd
.parse_playoff_series_games <- function(games_df, series_letter, playoff_round) {
    # The playoff-series endpoint returns startTimeUTC but no gameDate field.
    # Derive gameDate from startTimeUTC so .parse_club_schedule_games() can
    # produce game_date; mirrors the fallback in .parse_schedule_games().
    if (!"gameDate" %in% names(games_df) && "startTimeUTC" %in% names(games_df)) {
        games_df$gameDate <- as.character(as.Date(games_df$startTimeUTC))
    }

    base <- .parse_club_schedule_games(games_df)

    # Prefer an API-provided per-game number if present; otherwise derive
    # chronologically. The playoff-series endpoint returns games in date
    # order, so positional indexing yields the correct game number for
    # all completed games in the series.
    sgn <- if ("gameNumber" %in% names(games_df)) {
        as.integer(games_df$gameNumber)
    } else {
        seq_len(nrow(games_df))
    }

    base$series_letter <- as.character(series_letter)
    base$playoff_round <- as.integer(playoff_round)
    base$series_game_number <- sgn
    base
}


#' Empty 16-column schedule tibble with the right column types
#'
#' Used as a zero-row default when no games are available so downstream
#' `bind_rows()` calls don't widen or change column types.
#'
#' @return A zero-row tibble with all 16 schedule columns.
#' @keywords internal
#' @noRd
.empty_schedule_tibble <- function() {
    dplyr::tibble(
        game_id = integer(),
        season_full = character(),
        game_type = character(),
        game_date = character(),
        game_time = character(),
        home_team_abbr = character(),
        away_team_abbr = character(),
        home_team_name = character(),
        away_team_name = character(),
        home_score = integer(),
        away_score = integer(),
        game_state = character(),
        venue = character(),
        series_letter = character(),
        playoff_round = integer(),
        series_game_number = integer()
    )
}

#' Fetch all playoff games for a season as a tidy tibble
#'
#' Orchestrates the playoff fetch path used by `nhl_schedule()` when
#' `game_type` includes `"playoffs"`. Discovers series via the playoff
#' carousel, then fetches each per-series schedule and reshapes to the
#' schedule schema.
#'
#' @param season Integer end-year (e.g. `2024` = 2023-24 season).
#' @return 16-column tibble of playoff games (`game_type == "P"`), or an
#'   empty 16-column tibble if no playoff data exists for the season.
#' @keywords internal
#' @noRd
.fetch_nhl_season_playoffs <- function(season) {
    api_season <- paste0(season - 1, season)

    # Pass the 8-char form so nhl_playoff_carousel() uses it verbatim and we
    # avoid its end-year/start-year ambiguity.
    carousel <- tryCatch(
        nhl_playoff_carousel(season = api_season),
        error = function(e) NULL
    )

    series_map <- .extract_series_map(carousel)

    if (is.null(series_map) || nrow(series_map) == 0) {
        message(glue::glue(
            "{Sys.time()}: No playoff series found for season {api_season}"
        ))
        return(.empty_schedule_tibble())
    }

    pieces <- list()
    for (i in seq_len(nrow(series_map))) {
        letter <- series_map$series_letter[i]
        round_num <- series_map$playoff_round[i]
        raw <- .fetch_playoff_series(api_season, letter, flatten = FALSE)
        if (is.null(raw) || is.null(raw$games) || NROW(raw$games) == 0) {
            next
        }
        pieces[[letter]] <- .parse_playoff_series_games(
            raw$games,
            series_letter = letter,
            playoff_round = round_num
        )
    }

    if (length(pieces) == 0) {
        return(.empty_schedule_tibble())
    }

    dplyr::bind_rows(pieces)
}
