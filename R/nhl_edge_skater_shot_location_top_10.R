#' @title **NHL Edge Skater Shot Location Top 10**
#' @description Returns the NHL Edge top-10 skater leaderboard for a given
#'   shot location `position`, `category`, and `sort_by` combination. Wraps
#'   `https://api-web.nhle.com/v1/edge/skater-shot-location-top-10/{position}/{category}/{sortBy}/...`.
#'   When `season` is `NULL` (default) the `/now` endpoint is used to
#'   fetch the current season.
#' @param position Character position code accepted by the Edge API (e.g.,
#'   `"F"` for forwards, `"D"` for defensemen).
#' @param category Character shot-location category accepted by the Edge
#'   API (e.g., `"high"`, `"mid"`, `"low"`).
#' @param sort_by Character sort-by key accepted by the Edge API (e.g.,
#'   `"total"`).
#' @param season Optional 4-digit end-year (e.g., `2025` for the 2024-25
#'   season), an 8-character API season (e.g., `"20242025"`), or `NULL`
#'   (default) for the current season via the `/now` endpoint.
#' @param game_type Integer game type. 1 = preseason, 2 = regular season
#'   (default), 3 = playoffs.
#' @return A `fastRhockey_data` tibble with the top-10 leaderboard, or
#'   `NULL` on failure / empty response.
#' @keywords NHL Edge Skater
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \donttest{
#'   try(nhl_edge_skater_shot_location_top_10(
#'       position = "F",
#'       category = "high",
#'       sort_by = "total"
#'   ))
#' }
nhl_edge_skater_shot_location_top_10 <- function(
    position,
    category,
    sort_by,
    season = NULL,
    game_type = 2
) {
    raw <- .nhl_edge_api(
        base = glue::glue(
            "skater-shot-location-top-10/{position}/{category}/{sort_by}"
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
            "{Sys.time()}: No NHL Edge shot-location top-10 data returned for {position}/{category}/{sort_by}"
        ))
        return(NULL)
    }

    df <- janitor::clean_names(dplyr::as_tibble(df))
    make_fastRhockey_data(
        df,
        "NHL Edge Skater Shot Location Top 10",
        Sys.time()
    )
}
