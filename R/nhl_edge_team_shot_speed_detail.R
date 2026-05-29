#' @title **NHL Edge Team Shot Speed Detail**
#' @description Returns the NHL Edge shot-speed detail payload for a
#'   single team. Wraps
#'   `https://api-web.nhle.com/v1/edge/team-shot-speed-detail/{teamId}/...`.
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
#'    |col_name                                      |types     |description                                          |
#'    |:---------------------------------------------|:---------|:----------------------------------------------------|
#'    |game_center_link                              |character |Link to the NHL game center page for the game.       |
#'    |game_date                                     |character |Game date.                                           |
#'    |game_type                                     |integer   |Game type (1 = preseason, 2 = regular, 3 = playoffs).|
#'    |is_home_team                                  |logical   |Whether the team was the home team.                  |
#'    |time_in_period                                |character |Time elapsed in the period when the shot occurred.   |
#'    |player_id                                     |integer   |Unique player identifier.                            |
#'    |player_slug                                   |character |URL slug for the player.                             |
#'    |player_first_name_default                     |character |Player first name (default language).                |
#'    |player_last_name_default                      |character |Player last name (default language).                 |
#'    |player_last_name_cs                           |character |Player last name (Czech).                            |
#'    |player_last_name_fi                           |character |Player last name (Finnish).                          |
#'    |player_last_name_sk                           |character |Player last name (Slovak).                           |
#'    |shot_speed_imperial                           |numeric   |Shot speed in miles per hour.                        |
#'    |shot_speed_metric                             |numeric   |Shot speed in kilometers per hour.                   |
#'    |period_descriptor_number                      |integer   |Period number.                                       |
#'    |period_descriptor_period_type                 |character |Period type (e.g., REG, OT).                         |
#'    |period_descriptor_max_regulation_periods      |integer   |Maximum number of regulation periods.                |
#'    |home_team_common_name_default                 |character |Home team common name (default language).            |
#'    |home_team_place_name_with_preposition_default |character |Home team place name with preposition (default).     |
#'    |home_team_place_name_with_preposition_fr      |character |Home team place name with preposition (French).      |
#'    |home_team_team_logo_light                     |character |URL to the home team light logo.                     |
#'    |home_team_team_logo_dark                      |character |URL to the home team dark logo.                      |
#'    |away_team_common_name_default                 |character |Away team common name (default language).            |
#'    |away_team_place_name_with_preposition_default |character |Away team place name with preposition (default).     |
#'    |away_team_place_name_with_preposition_fr      |character |Away team place name with preposition (French).      |
#'    |away_team_team_logo_light                     |character |URL to the away team light logo.                     |
#'    |away_team_team_logo_dark                      |character |URL to the away team dark logo.                      |
#' @keywords NHL Edge Team
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \donttest{
#'   try(nhl_edge_team_shot_speed_detail(team_id = 10))
#' }
nhl_edge_team_shot_speed_detail <- function(
    team_id,
    season = NULL,
    game_type = 2
) {
    raw <- .nhl_edge_api(
        base = glue::glue("team-shot-speed-detail/{team_id}"),
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
            "{Sys.time()}: No NHL Edge team shot speed detail returned for team_id={team_id}"
        ))
        return(NULL)
    }

    df <- janitor::clean_names(dplyr::as_tibble(df))
    make_fastRhockey_data(df, "NHL Edge Team Shot Speed Detail", Sys.time())
}
