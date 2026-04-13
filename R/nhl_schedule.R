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
#'   try(nhl_schedule(day = "2024-01-15", include_data_flags = TRUE))
#' }
nhl_schedule <- function(day = NULL, season = NULL, team_abbr = NULL,
                         include_data_flags = FALSE) {
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

        if (!is.null(team_abbr)) {
            teams <- team_abbr
        } else {
            # Get all team abbreviations from internal data
            teams <- fastRhockey::nhl_team_logos$team_abbr
            if (is.null(teams) || length(teams) == 0) {
                teams <- c(
                    "ANA",
                    "ARI",
                    "BOS",
                    "BUF",
                    "CAR",
                    "CBJ",
                    "CGY",
                    "CHI",
                    "COL",
                    "DAL",
                    "DET",
                    "EDM",
                    "FLA",
                    "LAK",
                    "MIN",
                    "MTL",
                    "NJD",
                    "NSH",
                    "NYI",
                    "NYR",
                    "OTT",
                    "PHI",
                    "PIT",
                    "SEA",
                    "SJK",
                    "STL",
                    "TBL",
                    "TOR",
                    "UTA",
                    "VAN",
                    "VGK",
                    "WPG",
                    "WSH"
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

        if (length(all_games) == 0) {
            message(glue::glue(
                "{Sys.time()}: No games found for season {season_str}"
            ))
            return(NULL)
        }

        games_df <- dplyr::bind_rows(all_games)
        schedule <- .parse_club_schedule_games(games_df)
        schedule <- dplyr::distinct(
            schedule,
            .data$game_id,
            .keep_all = TRUE
        ) %>%
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
