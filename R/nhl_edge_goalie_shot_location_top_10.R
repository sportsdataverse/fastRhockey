#' @title **NHL Edge Goalie Shot Location Top 10**
#' @description Returns the NHL Edge top-10 goalie leaderboard for a given
#'   shot-location `category` and `sort_by` combination. Wraps
#'   `https://api-web.nhle.com/v1/edge/goalie-shot-location-top-10/{category}/{sortBy}/...`.
#'   When `season` is `NULL` (default) the `/now` endpoint is used to
#'   fetch the current season.
#' @param category Character shot-location category accepted by the Edge
#'   API (e.g., `"high"`, `"mid"`, `"low"`).
#' @param sort_by Character sort-by key accepted by the Edge API (e.g.,
#'   `"savePctg"`).
#' @param season Optional 4-digit end-year (e.g., `2025` for the 2024-25
#'   season), an 8-character API season (e.g., `"20242025"`), or `NULL`
#'   (default) for the current season via the `/now` endpoint.
#' @param game_type Integer game type. 1 = preseason, 2 = regular season
#'   (default), 3 = playoffs.
#' @return A `fastRhockey_data` tibble with the top-10 leaderboard, or
#'   `NULL` on failure / empty response.
#' @keywords NHL Edge Goalie
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \donttest{
#'   try(nhl_edge_goalie_shot_location_top_10(
#'       category = "high",
#'       sort_by = "savePctg"
#'   ))
#' }
nhl_edge_goalie_shot_location_top_10 <- function(
    category,
    sort_by,
    season = NULL,
    game_type = 2
) {
    raw <- .nhl_edge_api(
        base = glue::glue(
            "goalie-shot-location-top-10/{category}/{sort_by}"
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
            "{Sys.time()}: No NHL Edge goalie shot-location top-10 data returned for {category}/{sort_by}"
        ))
        return(NULL)
    }

    df <- janitor::clean_names(dplyr::as_tibble(df))
    make_fastRhockey_data(
        df,
        "NHL Edge Goalie Shot Location Top 10",
        Sys.time()
    )
}
