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
#' @return A `fastRhockey_data` tibble with the top-10 leaderboard, or
#'   `NULL` on failure / empty response.
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
