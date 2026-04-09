#' @title **NHL Edge Goalie Shot Location Detail**
#' @description Returns the NHL Edge shot-location detail payload for a
#'   single goalie. Wraps
#'   `https://api-web.nhle.com/v1/edge/goalie-shot-location-detail/{playerId}/...`.
#'   When `season` is `NULL` (default) the `/now` endpoint is used to
#'   fetch the current season.
#' @param player_id Integer NHL player ID (e.g., `8475883` for Andrei
#'   Vasilevskiy).
#' @param season Optional 4-digit end-year (e.g., `2025` for the 2024-25
#'   season), an 8-character API season (e.g., `"20242025"`), or `NULL`
#'   (default) for the current season via the `/now` endpoint.
#' @param game_type Integer game type. 1 = preseason, 2 = regular season
#'   (default), 3 = playoffs.
#' @return A `fastRhockey_data` tibble of shot-location details, or `NULL`
#'   on failure / empty response.
#' @keywords NHL Edge Goalie
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \donttest{
#'   try(nhl_edge_goalie_shot_location_detail(player_id = 8475883))
#' }
nhl_edge_goalie_shot_location_detail <- function(
    player_id,
    season = NULL,
    game_type = 2
) {
    raw <- .nhl_edge_api(
        base = glue::glue("goalie-shot-location-detail/{player_id}"),
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
            "{Sys.time()}: No NHL Edge goalie shot-location detail returned for player_id={player_id}"
        ))
        return(NULL)
    }

    df <- janitor::clean_names(dplyr::as_tibble(df))
    make_fastRhockey_data(
        df,
        "NHL Edge Goalie Shot Location Detail",
        Sys.time()
    )
}
