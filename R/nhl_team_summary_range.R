#' @title **NHL Team Summary (Season Range)**
#' @description Aggregator helper that calls [nhl_stats_teams()] with
#'   `report_type = "summary"` for every season in `[start_season,
#'   end_season]` and concatenates the results into a single tidy frame.
#'   Mirrors the `Stats.team_summary` convenience helper from the
#'   `nhl-api-py` Python client.
#' @param start_season Integer four-digit *end year* of the first season
#'   (e.g. `2022` for the 2021-22 season).
#' @param end_season Integer four-digit *end year* of the final season
#'   (inclusive). Must be `>= start_season`.
#' @param game_type Integer game type: `2` = regular season (default),
#'   `3` = playoffs.
#' @param limit Integer maximum number of rows per season request.
#'   Defaults to `50` to match [nhl_stats_teams()].
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                |types     |description                                  |
#'    |:-----------------------|:---------|:--------------------------------------------|
#'    |faceoff_win_pct         |numeric   |Faceoff win percentage.                      |
#'    |games_played            |integer   |Games played.                                |
#'    |goals_against           |integer   |Goals against.                               |
#'    |goals_against_per_game  |numeric   |Goals against per game.                       |
#'    |goals_for               |integer   |Goals for.                                   |
#'    |goals_for_per_game      |numeric   |Goals for per game.                          |
#'    |losses                  |integer   |Losses.                                      |
#'    |ot_losses               |integer   |Overtime losses.                             |
#'    |penalty_kill_net_pct    |numeric   |Net penalty kill percentage.                 |
#'    |penalty_kill_pct        |numeric   |Penalty kill percentage.                     |
#'    |point_pct               |numeric   |Points percentage.                           |
#'    |points                  |integer   |Total points.                               |
#'    |power_play_net_pct      |numeric   |Net power play percentage.                   |
#'    |power_play_pct          |numeric   |Power play percentage.                       |
#'    |regulation_and_ot_wins  |integer   |Wins in regulation and overtime.             |
#'    |season_id               |integer   |Season identifier.                           |
#'    |shots_against_per_game  |numeric   |Shots against per game.                      |
#'    |shots_for_per_game      |numeric   |Shots for per game.                          |
#'    |team_full_name          |character |Full team name.                              |
#'    |team_id                 |integer   |Unique team identifier.                      |
#'    |team_shutouts           |integer   |Team shutouts.                               |
#'    |ties                    |logical   |Ties (legacy; typically unused).             |
#'    |wins                    |integer   |Wins.                                        |
#'    |wins_in_regulation      |integer   |Wins in regulation.                          |
#'    |wins_in_shootout        |integer   |Wins in shootout.                            |
#'    |season                  |character |Season the row came from (YYYYYYYY).         |
#' @keywords NHL Helpers Aggregator
#' @importFrom dplyr bind_rows
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_team_summary_range(start_season = 2023, end_season = 2024))
#' }
nhl_team_summary_range <- function(
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
                    expr = nhl_stats_teams(
                        report_type = "summary",
                        season = api_season,
                        game_type = game_type,
                        limit = limit
                    ),
                    error = function(e) {
                        message(glue::glue(
                            "{Sys.time()}: Error fetching team summary for {api_season}: {e$message}"
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
                    "{Sys.time()}: No team summary data aggregated for {start_season}-{end_season}"
                ))
                return(NULL)
            }

            res <- dplyr::bind_rows(out)
            res <- make_fastRhockey_data(
                res,
                "NHL Team Summary Range",
                Sys.time()
            )
            return(res)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error aggregating NHL team summaries: {e$message}"
            ))
            return(NULL)
        }
    )
}
