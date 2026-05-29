#' @title **NHL Edge Skater Skating Distance Detail**
#' @description Returns the NHL Edge skating-distance detail payload for a
#'   single skater (distance covered, strides, etc.). Wraps
#'   `https://api-web.nhle.com/v1/edge/skater-skating-distance-detail/{playerId}/...`.
#'   When `season` is `NULL` (default) the `/now` endpoint is used to
#'   fetch the current season.
#' @param player_id Integer NHL player ID (e.g., `8478402` for Connor
#'   McDavid).
#' @param season Optional 4-digit end-year (e.g., `2025` for the 2024-25
#'   season), an 8-character API season (e.g., `"20242025"`), or `NULL`
#'   (default) for the current season via the `/now` endpoint.
#' @param game_type Integer game type. 1 = preseason, 2 = regular season
#'   (default), 3 = playoffs.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                                      |types     |description                                            |
#'    |:---------------------------------------------|:---------|:------------------------------------------------------|
#'    |game_center_link                              |character |Link to the NHL game center page.                      |
#'    |game_date                                     |character |Game date.                                             |
#'    |player_on_home_team                           |logical   |Whether the player was on the home team.               |
#'    |toi_all                                       |integer   |Total time on ice across all strengths (seconds).      |
#'    |toi_even                                      |integer   |Time on ice at even strength (seconds).                |
#'    |toi_pk                                        |integer   |Time on ice on the penalty kill (seconds).             |
#'    |toi_pp                                        |integer   |Time on ice on the power play (seconds).               |
#'    |distance_skated_all_imperial                  |numeric   |Distance skated at all strengths in miles.             |
#'    |distance_skated_all_metric                    |numeric   |Distance skated at all strengths in kilometers.        |
#'    |distance_skated_even_imperial                 |numeric   |Distance skated at even strength in miles.             |
#'    |distance_skated_even_metric                   |numeric   |Distance skated at even strength in kilometers.        |
#'    |distance_skated_pk_imperial                   |numeric   |Distance skated on the penalty kill in miles.          |
#'    |distance_skated_pk_metric                     |numeric   |Distance skated on the penalty kill in kilometers.     |
#'    |home_team_abbrev                              |character |Home team abbreviation.                                |
#'    |home_team_slug                                |character |Home team URL slug.                                    |
#'    |home_team_common_name_default                 |character |Home team common name.                                 |
#'    |home_team_place_name_with_preposition_default |character |Home team place name with preposition (English).       |
#'    |home_team_place_name_with_preposition_fr      |character |Home team place name with preposition (French).        |
#'    |home_team_team_logo_light                     |character |Home team light logo URL.                              |
#'    |home_team_team_logo_dark                      |character |Home team dark logo URL.                               |
#'    |away_team_abbrev                              |character |Away team abbreviation.                                |
#'    |away_team_slug                                |character |Away team URL slug.                                    |
#'    |away_team_common_name_default                 |character |Away team common name.                                 |
#'    |away_team_place_name_with_preposition_default |character |Away team place name with preposition (English).       |
#'    |away_team_place_name_with_preposition_fr      |character |Away team place name with preposition (French).        |
#'    |away_team_team_logo_light                     |character |Away team light logo URL.                              |
#'    |away_team_team_logo_dark                      |character |Away team dark logo URL.                               |
#'    |distance_skated_pp_imperial                   |numeric   |Distance skated on the power play in miles.            |
#'    |distance_skated_pp_metric                     |numeric   |Distance skated on the power play in kilometers.       |
#' @keywords NHL Edge Skater
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \donttest{
#'   try(nhl_edge_skater_skating_distance_detail(player_id = 8478402))
#' }
nhl_edge_skater_skating_distance_detail <- function(
    player_id,
    season = NULL,
    game_type = 2
) {
    raw <- .nhl_edge_api(
        base = glue::glue("skater-skating-distance-detail/{player_id}"),
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
            "{Sys.time()}: No NHL Edge skating-distance detail returned for player_id={player_id}"
        ))
        return(NULL)
    }

    df <- janitor::clean_names(dplyr::as_tibble(df))
    make_fastRhockey_data(
        df,
        "NHL Edge Skater Skating Distance Detail",
        Sys.time()
    )
}
