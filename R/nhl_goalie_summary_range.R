#' @title **NHL Goalie Summary (Season Range)**
#' @description Aggregator helper that calls [nhl_stats_goalies()] with
#'   `report_type = "summary"` for every season in `[start_season,
#'   end_season]` and concatenates the results into a single tidy frame.
#'   Mirrors the `Stats.goalie_stats_summary` convenience helper from
#'   the `nhl-api-py` Python client.
#' @param start_season Integer four-digit *end year* of the first season
#'   (e.g. `2022` for the 2021-22 season).
#' @param end_season Integer four-digit *end year* of the final season
#'   (inclusive). Must be `>= start_season`.
#' @param game_type Integer game type: `2` = regular season (default),
#'   `3` = playoffs.
#' @param limit Integer maximum number of rows per season request.
#'   Defaults to `50` to match [nhl_stats_goalies()].
#' @return A `fastRhockey_data` / `data.frame` of concatenated goalie
#'   summary stats with an added `season` column marking the season
#'   each row came from. Returns `NULL` on outer failure.
#' @keywords NHL Helpers Aggregator
#' @importFrom dplyr bind_rows
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_goalie_summary_range(start_season = 2023, end_season = 2024))
#' }
nhl_goalie_summary_range <- function(
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
                    expr = nhl_stats_goalies(
                        report_type = "summary",
                        season = api_season,
                        game_type = game_type,
                        limit = limit
                    ),
                    error = function(e) {
                        message(glue::glue(
                            "{Sys.time()}: Error fetching goalie summary for {api_season}: {e$message}"
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
                    "{Sys.time()}: No goalie summary data aggregated for {start_season}-{end_season}"
                ))
                return(NULL)
            }

            res <- dplyr::bind_rows(out)
            res <- make_fastRhockey_data(
                res,
                "NHL Goalie Summary Range",
                Sys.time()
            )
            return(res)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error aggregating NHL goalie summaries: {e$message}"
            ))
            return(NULL)
        }
    )
}
