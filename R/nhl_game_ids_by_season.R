#' @title **NHL Game IDs by Season**
#' @description Aggregator helper that iterates every NHL team's
#'   season schedule (via [nhl_club_schedule()]) and returns a deduplicated
#'   tidy data frame of game IDs for the requested season and game types.
#'   Mirrors the `Helpers.game_ids_by_season` convenience helper from the
#'   `nhl-api-py` Python client.
#' @param season Integer four-digit *end year* of the season (e.g. `2025`
#'   for the 2024-25 season). Passed through to [nhl_club_schedule()] which
#'   uses the same end-year convention.
#' @param game_types Integer vector of NHL game types to keep.
#'   `1` = preseason, `2` = regular season (default), `3` = playoffs.
#' @param team_abbr Optional character three-letter team abbreviation
#'   (e.g. `"TOR"`). If supplied, only that team's schedule is fetched
#'   instead of iterating all 32 NHL clubs.
#' @param sleep_rate Numeric seconds to `Sys.sleep()` between per-team
#'   API calls when iterating all teams. Defaults to `0` (no delay).
#' @return A `fastRhockey_data` / `data.frame` with one row per unique
#'   game, containing `game_id`, `season`, `game_type`, `game_date`,
#'   `home_team`, and `away_team`. Returns `NULL` on outer failure.
#' @keywords NHL Helpers Aggregator
#' @importFrom dplyr bind_rows distinct filter select tibble
#' @importFrom glue glue
#' @importFrom rlang .data
#' @export
#' @examples
#' \donttest{
#'   try(nhl_game_ids_by_season(season = 2025, team_abbr = "TOR"))
#' }
nhl_game_ids_by_season <- function(
    season,
    game_types = c(2L),
    team_abbr = NULL,
    sleep_rate = 0
) {
    tryCatch(
        expr = {
            if (is.null(team_abbr)) {
                teams_df <- nhl_teams()
                if (is.null(teams_df) || nrow(teams_df) == 0) {
                    message(glue::glue(
                        "{Sys.time()}: Unable to fetch NHL teams list"
                    ))
                    return(NULL)
                }
                team_list <- unique(teams_df$team_abbr)
            } else {
                team_list <- team_abbr
            }

            schedules <- list()
            for (i in seq_along(team_list)) {
                tm <- team_list[i]
                sched <- tryCatch(
                    expr = nhl_club_schedule(
                        team_abbr = tm,
                        season = season
                    ),
                    error = function(e) {
                        message(glue::glue(
                            "{Sys.time()}: Error fetching schedule for {tm}: {e$message}"
                        ))
                        return(NULL)
                    }
                )

                if (!is.null(sched) && nrow(sched) > 0) {
                    nms <- names(sched)

                    pick <- function(candidates) {
                        hit <- candidates[candidates %in% nms]
                        if (length(hit) == 0) {
                            return(rep(NA, nrow(sched)))
                        }
                        sched[[hit[1]]]
                    }

                    game_id_vec <- pick(c("id", "game_id"))
                    season_vec <- pick(c("season"))
                    game_type_vec <- pick(c("game_type", "game_type_id"))
                    game_date_vec <- pick(c("game_date", "start_time_utc"))
                    home_vec <- pick(c(
                        "home_team_abbrev",
                        "home_team_abbr",
                        "home_team"
                    ))
                    away_vec <- pick(c(
                        "away_team_abbrev",
                        "away_team_abbr",
                        "away_team"
                    ))

                    schedules[[length(schedules) + 1]] <- dplyr::tibble(
                        game_id = game_id_vec,
                        season = season_vec,
                        game_type = game_type_vec,
                        game_date = game_date_vec,
                        home_team = home_vec,
                        away_team = away_vec
                    )
                }

                if (is.null(team_abbr) && sleep_rate > 0 && i < length(team_list)) {
                    Sys.sleep(sleep_rate)
                }
            }

            if (length(schedules) == 0) {
                message(glue::glue(
                    "{Sys.time()}: No schedule data aggregated for season {season}"
                ))
                return(NULL)
            }

            df <- dplyr::bind_rows(schedules)

            if (!is.null(game_types) && length(game_types) > 0) {
                df <- df %>%
                    dplyr::filter(.data$game_type %in% as.integer(game_types))
            }

            df <- df %>%
                dplyr::distinct(.data$game_id, .keep_all = TRUE)

            df <- make_fastRhockey_data(
                df,
                "NHL Game IDs by Season",
                Sys.time()
            )
            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error aggregating NHL game IDs: {e$message}"
            ))
            return(NULL)
        }
    )
}
