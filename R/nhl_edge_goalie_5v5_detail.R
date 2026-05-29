#' @title **NHL Edge Goalie 5v5 Detail**
#' @description Returns the NHL Edge 5-on-5 advanced-metrics detail payload
#'   for a single goalie. Wraps
#'   `https://api-web.nhle.com/v1/edge/goalie-5v5-detail/{playerId}/...`.
#'   When `season` is `NULL` (default) the `/now` endpoint is used to
#'   fetch the current season.
#' @param player_id Integer NHL player ID (e.g., `8475883` for Andrei
#'   Vasilevskiy).
#' @param season Optional 4-digit end-year (e.g., `2025` for the 2024-25
#'   season), an 8-character API season (e.g., `"20242025"`), or `NULL`
#'   (default) for the current season via the `/now` endpoint.
#' @param game_type Integer game type. 1 = preseason, 2 = regular season
#'   (default), 3 = playoffs.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                                      |types     |description                                  |
#'    |:---------------------------------------------|:---------|:--------------------------------------------|
#'    |game_center_link                              |character |URL to the game center page.                 |
#'    |save_pctg                                     |numeric   |Save percentage.                             |
#'    |game_date                                     |character |Game date.                                   |
#'    |decision                                      |character |Goalie decision (W/L/OT).                    |
#'    |player_on_home_team                           |logical   |Whether the goalie played for the home team. |
#'    |home_team_abbrev                              |character |Home team abbreviation.                      |
#'    |home_team_slug                                |character |Home team slug.                              |
#'    |home_team_common_name_default                 |character |Home team common name (default language).    |
#'    |home_team_common_name_fr                      |character |Home team common name (French).              |
#'    |home_team_place_name_with_preposition_default |character |Home team place name with preposition (def). |
#'    |home_team_place_name_with_preposition_fr      |character |Home team place name with preposition (FR).  |
#'    |home_team_team_logo_light                     |character |Home team logo URL (light variant).          |
#'    |home_team_team_logo_dark                      |character |Home team logo URL (dark variant).           |
#'    |away_team_abbrev                              |character |Away team abbreviation.                      |
#'    |away_team_slug                                |character |Away team slug.                              |
#'    |away_team_common_name_default                 |character |Away team common name (default language).    |
#'    |away_team_place_name_with_preposition_default |character |Away team place name with preposition (def). |
#'    |away_team_place_name_with_preposition_fr      |character |Away team place name with preposition (FR).  |
#'    |away_team_team_logo_light                     |character |Away team logo URL (light variant).          |
#'    |away_team_team_logo_dark                      |character |Away team logo URL (dark variant).           |
#' @keywords NHL Edge Goalie
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \donttest{
#'   try(nhl_edge_goalie_5v5_detail(player_id = 8475883))
#' }
nhl_edge_goalie_5v5_detail <- function(
    player_id,
    season = NULL,
    game_type = 2
) {
    raw <- .nhl_edge_api(
        base = glue::glue("goalie-5v5-detail/{player_id}"),
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
            "{Sys.time()}: No NHL Edge goalie 5v5 detail returned for player_id={player_id}"
        ))
        return(NULL)
    }

    df <- janitor::clean_names(dplyr::as_tibble(df))
    make_fastRhockey_data(df, "NHL Edge Goalie 5v5 Detail", Sys.time())
}
