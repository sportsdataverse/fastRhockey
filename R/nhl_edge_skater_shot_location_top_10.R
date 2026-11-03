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
#' @return `NULL`, always: the NHL removed this endpoint, so the function
#'   no longer sends a request.
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
    lifecycle::deprecate_warn(
        "1.0.0",
        "nhl_edge_skater_shot_location_top_10()",
        details = paste(
            "The NHL has removed this Edge top-10 leaderboard endpoint;",
            "the function returns NULL."
        )
    )

    # The NHL removed this endpoint, so no request is sent.
    NULL
}
