#' @title **NHL Edge Team Zone Time Details**
#' @description Returns the NHL Edge zone-time details payload for a
#'   single team. Wraps
#'   `https://api-web.nhle.com/v1/edge/team-zone-time-details/{teamId}/...`.
#'   When `season` is `NULL` (default) the `/now` endpoint is used to fetch
#'   the current season.
#' @param team_id Integer NHL team ID (e.g., `10` for Toronto Maple Leafs).
#' @param season Optional 4-digit end-year (e.g., `2025` for the 2024-25
#'   season), an 8-character API season (e.g., `"20242025"`), or `NULL`
#'   (default) for the current season via the `/now` endpoint.
#' @param game_type Integer game type. 1 = preseason, 2 = regular season
#'   (default), 3 = playoffs.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                  |types     |description                                          |
#'    |:-------------------------|:---------|:----------------------------------------------------|
#'    |strength_code             |character |Game-strength state the row applies to.              |
#'    |offensive_zone_pctg       |numeric   |Percentage of time spent in the offensive zone.      |
#'    |offensive_zone_rank       |integer   |League rank for offensive zone time.                 |
#'    |offensive_zone_league_avg |numeric   |League-average offensive zone time percentage.       |
#'    |neutral_zone_pctg         |numeric   |Percentage of time spent in the neutral zone.        |
#'    |neutral_zone_rank         |integer   |League rank for neutral zone time.                   |
#'    |neutral_zone_league_avg   |numeric   |League-average neutral zone time percentage.         |
#'    |defensive_zone_pctg       |numeric   |Percentage of time spent in the defensive zone.      |
#'    |defensive_zone_rank       |integer   |League rank for defensive zone time.                 |
#'    |defensive_zone_league_avg |numeric   |League-average defensive zone time percentage.       |
#' @keywords NHL Edge Team
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \donttest{
#'   try(nhl_edge_team_zone_time_details(team_id = 10))
#' }
nhl_edge_team_zone_time_details <- function(
    team_id,
    season = NULL,
    game_type = 2
) {
    raw <- .nhl_edge_api(
        base = glue::glue("team-zone-time-details/{team_id}"),
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
            "{Sys.time()}: No NHL Edge team zone time details returned for team_id={team_id}"
        ))
        return(NULL)
    }

    df <- janitor::clean_names(dplyr::as_tibble(df))
    make_fastRhockey_data(df, "NHL Edge Team Zone Time Details", Sys.time())
}
