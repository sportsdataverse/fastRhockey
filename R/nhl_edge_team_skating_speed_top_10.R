#' @title **NHL Edge Team Skating Speed Top 10**
#' @description Returns the NHL Edge top-10 team skating-speed leaderboard.
#'   Wraps
#'   `https://api-web.nhle.com/v1/edge/team-skating-speed-top-10/{positions}/{sortBy}/...`.
#'   When `season` is `NULL` (default) the `/now` endpoint is used to fetch
#'   the current season.
#' @param positions Character position filter (e.g., `"F"`, `"D"`, `"all"`).
#' @param sort_by Character metric to sort the leaderboard by (e.g.,
#'   `"total"`, `"top_speed"`).
#' @param season Optional 4-digit end-year (e.g., `2025` for the 2024-25
#'   season), an 8-character API season (e.g., `"20242025"`), or `NULL`
#'   (default) for the current season via the `/now` endpoint.
#' @param game_type Integer game type. 1 = preseason, 2 = regular season
#'   (default), 3 = playoffs.
#' @return A `fastRhockey_data` tibble with the top-10 team leaderboard, or
#'   `NULL` on failure / empty response.
#' @keywords NHL Edge Team
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \donttest{
#'   try(nhl_edge_team_skating_speed_top_10(
#'     positions = "F",
#'     sort_by = "total"
#'   ))
#' }
nhl_edge_team_skating_speed_top_10 <- function(
    positions,
    sort_by,
    season = NULL,
    game_type = 2
) {
    raw <- .nhl_edge_api(
        base = glue::glue(
            "team-skating-speed-top-10/{positions}/{sort_by}"
        ),
        season = season,
        game_type = game_type,
        prefix = "v1/edge"
    )
    if (is.null(raw)) {
        return(NULL)
    }

    df <- .nhl_edge_to_df(raw)
    if (is.null(df) || (is.data.frame(df) && nrow(df) == 0)) {
        message(glue::glue(
            "{Sys.time()}: No NHL Edge team skating speed top 10 returned"
        ))
        return(NULL)
    }

    df <- janitor::clean_names(dplyr::as_tibble(df))
    make_fastRhockey_data(df, "NHL Edge Team Skating Speed Top 10", Sys.time())
}
