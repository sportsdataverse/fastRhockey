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
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                                       |types     |description                                        |
#'    |:----------------------------------------------|:---------|:--------------------------------------------------|
#'    |game_center_link                               |character |Link to the NHL game center page.                  |
#'    |game_date                                      |character |Game date.                                         |
#'    |game_type                                      |integer   |Game type code (2 = regular, 3 = playoffs).        |
#'    |is_home_team                                   |logical   |Whether the player's team was the home team.       |
#'    |time_in_period                                 |character |Time in the period the top speed was recorded.     |
#'    |player_id                                      |integer   |Unique player identifier.                          |
#'    |player_slug                                    |character |Player URL slug.                                   |
#'    |player_first_name_default                      |character |Player first name.                                 |
#'    |player_last_name_default                       |character |Player last name.                                  |
#'    |skating_speed_imperial                         |numeric   |Top skating speed in miles per hour.               |
#'    |skating_speed_metric                           |numeric   |Top skating speed in kilometers per hour.          |
#'    |period_descriptor_number                       |integer   |Period number.                                     |
#'    |period_descriptor_period_type                  |character |Period type (REG, OT, SO).                         |
#'    |period_descriptor_max_regulation_periods       |integer   |Maximum number of regulation periods.              |
#'    |home_team_common_name_default                  |character |Home team common name.                             |
#'    |home_team_place_name_with_preposition_default  |character |Home team place name with preposition.             |
#'    |home_team_place_name_with_preposition_fr       |character |Home team place name with preposition (French).    |
#'    |home_team_team_logo_light                      |character |Home team light logo URL.                          |
#'    |home_team_team_logo_dark                       |character |Home team dark logo URL.                           |
#'    |away_team_common_name_default                  |character |Away team common name.                             |
#'    |away_team_place_name_with_preposition_default  |character |Away team place name with preposition.             |
#'    |away_team_place_name_with_preposition_fr       |character |Away team place name with preposition (French).    |
#'    |away_team_team_logo_light                      |character |Away team light logo URL.                          |
#'    |away_team_team_logo_dark                       |character |Away team dark logo URL.                           |
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
