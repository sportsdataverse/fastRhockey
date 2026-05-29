#' @title **NHL Skater Summary (Season Range)**
#' @description Aggregator helper that calls [nhl_stats_skaters()] with
#'   `report_type = "summary"` for every season in `[start_season,
#'   end_season]` and concatenates the results into a single tidy frame.
#'   Mirrors the `Stats.skater_stats_summary` convenience helper from
#'   the `nhl-api-py` Python client.
#' @param start_season Integer four-digit *end year* of the first season
#'   (e.g. `2022` for the 2021-22 season).
#' @param end_season Integer four-digit *end year* of the final season
#'   (inclusive). Must be `>= start_season`.
#' @param game_type Integer game type: `2` = regular season (default),
#'   `3` = playoffs.
#' @param limit Integer maximum number of rows per season request.
#'   Defaults to `50` to match [nhl_stats_skaters()].
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name             |types     |description                |
#'    |:--------------------|:---------|:--------------------------|
#'    |assists              |integer   |Assists.                   |
#'    |ev_goals             |integer   |Even-strength goals.       |
#'    |ev_points            |integer   |Even-strength points.      |
#'    |faceoff_win_pct      |numeric   |Faceoff win percentage.    |
#'    |game_winning_goals   |integer   |Game-winning goals.        |
#'    |games_played         |integer   |Games played.              |
#'    |goals                |integer   |Goals scored.              |
#'    |last_name            |character |Player last name.          |
#'    |ot_goals             |integer   |Overtime goals.            |
#'    |penalty_minutes      |integer   |Penalty minutes.           |
#'    |player_id            |integer   |Unique player identifier.  |
#'    |plus_minus           |integer   |Plus/minus rating.         |
#'    |points               |integer   |Total points (goals + assists). |
#'    |points_per_game      |numeric   |Points per game.           |
#'    |position_code        |character |Player position code.      |
#'    |pp_goals             |integer   |Power-play goals.          |
#'    |pp_points            |integer   |Power-play points.         |
#'    |season_id            |integer   |Season identifier.         |
#'    |sh_goals             |integer   |Short-handed goals.        |
#'    |sh_points            |integer   |Short-handed points.       |
#'    |shooting_pct         |numeric   |Shooting percentage.       |
#'    |shoots_catches       |character |Handedness (shoots/catches).|
#'    |shots                |integer   |Shots on goal.             |
#'    |skater_full_name     |character |Player full name.          |
#'    |team_abbrevs         |character |Team abbreviation(s).      |
#'    |time_on_ice_per_game |numeric   |Average time on ice per game. |
#'    |season               |character |Season the row came from (e.g., "20232024"). |
#'
#' Returns `NULL` on outer failure.
#' @keywords NHL Helpers Aggregator
#' @importFrom dplyr bind_rows
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_skater_summary_range(start_season = 2023, end_season = 2024))
#' }
nhl_skater_summary_range <- function(
    start_season,
    end_season,
    game_type = 2,
    limit = 50
) {
    tryCatch(
        expr = {
            if (!is.numeric(start_season) || !is.numeric(end_season)) {
                stop("start_season and end_season must be numeric end years")
            }
            if (start_season > end_season) {
                stop("start_season must be <= end_season")
            }

            seasons <- seq.int(
                as.integer(start_season),
                as.integer(end_season)
            )

            out <- list()
            for (s in seasons) {
                api_season <- paste0(s - 1, s)
                df <- tryCatch(
                    expr = nhl_stats_skaters(
                        report_type = "summary",
                        season = api_season,
                        game_type = game_type,
                        limit = limit
                    ),
                    error = function(e) {
                        message(glue::glue(
                            "{Sys.time()}: Error fetching skater summary for {api_season}: {e$message}"
                        ))
                        return(NULL)
                    }
                )
                if (!is.null(df) && nrow(df) > 0) {
                    df$season <- api_season
                    out[[length(out) + 1]] <- df
                }
            }

            if (length(out) == 0) {
                message(glue::glue(
                    "{Sys.time()}: No skater summary data aggregated for {start_season}-{end_season}"
                ))
                return(NULL)
            }

            res <- dplyr::bind_rows(out)
            res <- make_fastRhockey_data(
                res,
                "NHL Skater Summary Range",
                Sys.time()
            )
            return(res)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error aggregating NHL skater summaries: {e$message}"
            ))
            return(NULL)
        }
    )
}
