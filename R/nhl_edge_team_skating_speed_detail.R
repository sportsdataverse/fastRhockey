#' @title **NHL Edge Team Skating Speed Detail**
#' @description Returns the NHL Edge skating-speed detail payload for a
#'   single team. Wraps
#'   `https://api-web.nhle.com/v1/edge/team-skating-speed-detail/{teamId}/...`.
#'   When `season` is `NULL` (default) the `/now` endpoint is used to fetch
#'   the current season.
#' @param team_id Integer NHL team ID (e.g., `10` for Toronto Maple Leafs).
#' @param season Optional 4-digit end-year (e.g., `2025` for the 2024-25
#'   season), an 8-character API season (e.g., `"20242025"`), or `NULL`
#'   (default) for the current season via the `/now` endpoint.
#' @param game_type Integer game type. 1 = preseason, 2 = regular season
#'   (default), 3 = playoffs.
#' @return A `fastRhockey_data` tibble of skating-speed metrics, or `NULL`
#'   on failure / empty response.
#' @keywords NHL Edge Team
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \donttest{
#'   try(nhl_edge_team_skating_speed_detail(team_id = 10))
#' }
nhl_edge_team_skating_speed_detail <- function(
    team_id,
    season = NULL,
    game_type = 2
) {
    raw <- .nhl_edge_api(
        base = glue::glue("team-skating-speed-detail/{team_id}"),
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
            "{Sys.time()}: No NHL Edge team skating speed detail returned for team_id={team_id}"
        ))
        return(NULL)
    }

    df <- janitor::clean_names(dplyr::as_tibble(df))
    make_fastRhockey_data(df, "NHL Edge Team Skating Speed Detail", Sys.time())
}
