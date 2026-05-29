#' @title **NHL Edge Skater Distance Top 10**
#' @description Returns the NHL Edge top-10 "iron-man / mileage" skating
#'   distance leaderboard for a given `positions`, `strength`, and
#'   `sort_by` combination. Wraps
#'   `https://api-web.nhle.com/v1/edge/skater-distance-top-10/{positions}/{strength}/{sortBy}/...`.
#'   When `season` is `NULL` (default) the `/now` endpoint is used to
#'   fetch the current season.
#' @param positions Character positions filter accepted by the Edge API
#'   (e.g., `"F"` for forwards, `"D"` for defensemen).
#' @param strength Character strength filter accepted by the Edge API
#'   (e.g., `"all"`, `"ev"`, `"pp"`, `"sh"`).
#' @param sort_by Character sort-by key accepted by the Edge API (e.g.,
#'   `"total"`).
#' @param season Optional 4-digit end-year (e.g., `2025` for the 2024-25
#'   season), an 8-character API season (e.g., `"20242025"`), or `NULL`
#'   (default) for the current season via the `/now` endpoint.
#' @param game_type Integer game type. 1 = preseason, 2 = regular season
#'   (default), 3 = playoffs.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                                                                  |types     |description                                          |
#'    |:-------------------------------------------------------------------------|:---------|:----------------------------------------------------|
#'    |player_slug                                                               |character |URL-friendly player slug.                            |
#'    |player_headshot                                                           |character |Player headshot image URL.                           |
#'    |player_position                                                           |character |Player position.                                     |
#'    |player_sweater_number                                                     |integer   |Jersey number.                                       |
#'    |player_first_name_default                                                 |character |Player first name (default locale).                  |
#'    |player_first_name_cs                                                      |character |Player first name (Czech locale).                    |
#'    |player_first_name_de                                                      |character |Player first name (German locale).                   |
#'    |player_first_name_es                                                      |character |Player first name (Spanish locale).                  |
#'    |player_first_name_fi                                                      |character |Player first name (Finnish locale).                  |
#'    |player_first_name_sk                                                      |character |Player first name (Slovak locale).                   |
#'    |player_first_name_sv                                                      |character |Player first name (Swedish locale).                  |
#'    |player_last_name_default                                                  |character |Player last name (default locale).                   |
#'    |player_last_name_cs                                                       |character |Player last name (Czech locale).                     |
#'    |player_last_name_sk                                                       |character |Player last name (Slovak locale).                    |
#'    |player_team_abbrev                                                        |character |Player team abbreviation.                            |
#'    |player_team_slug                                                          |character |Player team URL-friendly slug.                       |
#'    |player_team_common_name_default                                           |character |Player team common name (default locale).            |
#'    |player_team_place_name_with_preposition_default                           |character |Player team place name with preposition (default locale).|
#'    |player_team_place_name_with_preposition_fr                                |character |Player team place name with preposition (French locale).|
#'    |player_team_team_logo_light                                               |character |Player team light-mode logo URL.                     |
#'    |player_team_team_logo_dark                                                |character |Player team dark-mode logo URL.                      |
#'    |distance_total_imperial                                                   |numeric   |Total skating distance (imperial units).             |
#'    |distance_total_metric                                                     |numeric   |Total skating distance (metric units).               |
#'    |distance_per60_imperial                                                   |numeric   |Skating distance per 60 minutes (imperial units).    |
#'    |distance_per60_metric                                                     |numeric   |Skating distance per 60 minutes (metric units).      |
#'    |distance_max_per_game_imperial                                            |numeric   |Maximum single-game skating distance (imperial units).|
#'    |distance_max_per_game_metric                                              |numeric   |Maximum single-game skating distance (metric units). |
#'    |distance_max_per_game_overlay_game_date                                   |character |Game date of the max-per-game performance.           |
#'    |distance_max_per_game_overlay_game_type                                   |integer   |Game type of the max-per-game performance.           |
#'    |distance_max_per_game_overlay_player_first_name_default                   |character |Max-per-game player first name (default locale).     |
#'    |distance_max_per_game_overlay_player_first_name_cs                        |character |Max-per-game player first name (Czech locale).       |
#'    |distance_max_per_game_overlay_player_first_name_de                        |character |Max-per-game player first name (German locale).      |
#'    |distance_max_per_game_overlay_player_first_name_es                        |character |Max-per-game player first name (Spanish locale).     |
#'    |distance_max_per_game_overlay_player_first_name_fi                        |character |Max-per-game player first name (Finnish locale).     |
#'    |distance_max_per_game_overlay_player_first_name_sk                        |character |Max-per-game player first name (Slovak locale).      |
#'    |distance_max_per_game_overlay_player_first_name_sv                        |character |Max-per-game player first name (Swedish locale).     |
#'    |distance_max_per_game_overlay_player_last_name_default                    |character |Max-per-game player last name (default locale).      |
#'    |distance_max_per_game_overlay_player_last_name_cs                         |character |Max-per-game player last name (Czech locale).        |
#'    |distance_max_per_game_overlay_player_last_name_sk                         |character |Max-per-game player last name (Slovak locale).       |
#'    |distance_max_per_game_overlay_away_team_abbrev                            |character |Away team abbreviation in the max-per-game game.     |
#'    |distance_max_per_game_overlay_away_team_score                             |integer   |Away team score in the max-per-game game.            |
#'    |distance_max_per_game_overlay_home_team_abbrev                            |character |Home team abbreviation in the max-per-game game.     |
#'    |distance_max_per_game_overlay_home_team_score                             |integer   |Home team score in the max-per-game game.            |
#'    |distance_max_per_game_overlay_game_outcome_last_period_type               |character |Last period type of the max-per-game game outcome.   |
#'    |distance_max_per_game_overlay_game_outcome_ot_periods                     |integer   |Number of overtime periods in the max-per-game game. |
#'    |distance_max_per_game_overlay_period_descriptor_max_regulation_periods    |integer   |Maximum regulation periods for the max-per-game game.|
#'    |distance_max_per_game_overlay_period_descriptor_number                    |integer   |Period number for the max-per-game descriptor.       |
#'    |distance_max_per_game_overlay_period_descriptor_period_type               |character |Period type for the max-per-game descriptor.         |
#'    |distance_max_per_period_imperial                                          |numeric   |Maximum single-period skating distance (imperial units).|
#'    |distance_max_per_period_metric                                            |numeric   |Maximum single-period skating distance (metric units).|
#'    |distance_max_per_period_overlay_game_date                                 |character |Game date of the max-per-period performance.         |
#'    |distance_max_per_period_overlay_game_type                                 |integer   |Game type of the max-per-period performance.         |
#'    |distance_max_per_period_overlay_player_first_name_default                 |character |Max-per-period player first name (default locale).   |
#'    |distance_max_per_period_overlay_player_first_name_cs                      |character |Max-per-period player first name (Czech locale).     |
#'    |distance_max_per_period_overlay_player_first_name_de                      |character |Max-per-period player first name (German locale).    |
#'    |distance_max_per_period_overlay_player_first_name_es                      |character |Max-per-period player first name (Spanish locale).   |
#'    |distance_max_per_period_overlay_player_first_name_fi                      |character |Max-per-period player first name (Finnish locale).   |
#'    |distance_max_per_period_overlay_player_first_name_sk                      |character |Max-per-period player first name (Slovak locale).    |
#'    |distance_max_per_period_overlay_player_first_name_sv                      |character |Max-per-period player first name (Swedish locale).   |
#'    |distance_max_per_period_overlay_player_last_name_default                  |character |Max-per-period player last name (default locale).    |
#'    |distance_max_per_period_overlay_player_last_name_cs                       |character |Max-per-period player last name (Czech locale).      |
#'    |distance_max_per_period_overlay_player_last_name_sk                       |character |Max-per-period player last name (Slovak locale).     |
#'    |distance_max_per_period_overlay_away_team_abbrev                          |character |Away team abbreviation in the max-per-period game.   |
#'    |distance_max_per_period_overlay_away_team_score                           |integer   |Away team score in the max-per-period game.          |
#'    |distance_max_per_period_overlay_home_team_abbrev                          |character |Home team abbreviation in the max-per-period game.   |
#'    |distance_max_per_period_overlay_home_team_score                           |integer   |Home team score in the max-per-period game.          |
#'    |distance_max_per_period_overlay_game_outcome_last_period_type             |character |Last period type of the max-per-period game outcome. |
#'    |distance_max_per_period_overlay_game_outcome_ot_periods                   |integer   |Number of overtime periods in the max-per-period game.|
#'    |distance_max_per_period_overlay_period_descriptor_max_regulation_periods  |integer   |Maximum regulation periods for the max-per-period game.|
#'    |distance_max_per_period_overlay_period_descriptor_number                  |integer   |Period number for the max-per-period descriptor.     |
#'    |distance_max_per_period_overlay_period_descriptor_period_type             |character |Period type for the max-per-period descriptor.       |
#'
#'   Returns `NULL` on failure / empty response.
#' @keywords NHL Edge Skater
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \donttest{
#'   try(nhl_edge_skater_distance_top_10(
#'       positions = "F",
#'       strength = "all",
#'       sort_by = "total"
#'   ))
#' }
nhl_edge_skater_distance_top_10 <- function(
    positions,
    strength,
    sort_by,
    season = NULL,
    game_type = 2
) {
    raw <- .nhl_edge_api(
        base = glue::glue(
            "skater-distance-top-10/{positions}/{strength}/{sort_by}"
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
            "{Sys.time()}: No NHL Edge skating-distance top-10 data returned for {positions}/{strength}/{sort_by}"
        ))
        return(NULL)
    }

    df <- janitor::clean_names(dplyr::as_tibble(df))
    make_fastRhockey_data(
        df,
        "NHL Edge Skater Distance Top 10",
        Sys.time()
    )
}
