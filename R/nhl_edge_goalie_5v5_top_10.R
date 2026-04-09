#' @title **NHL Edge Goalie 5v5 Top 10**
#' @description Returns the NHL Edge top-10 5-on-5 goalie leaderboard for a
#'   given `sort_by` key. Wraps
#'   `https://api-web.nhle.com/v1/edge/goalie-5v5-top-10/{sortBy}/...`.
#'   When `season` is `NULL` (default) the `/now` endpoint is used to
#'   fetch the current season.
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
#'   try(nhl_edge_goalie_5v5_top_10(sort_by = "savePctg"))
#' }
nhl_edge_goalie_5v5_top_10 <- function(
    sort_by,
    season = NULL,
    game_type = 2
) {
    raw <- .nhl_edge_api(
        base = glue::glue("goalie-5v5-top-10/{sort_by}"),
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
            "{Sys.time()}: No NHL Edge goalie 5v5 top-10 data returned for {sort_by}"
        ))
        return(NULL)
    }

    df <- janitor::clean_names(dplyr::as_tibble(df))
    make_fastRhockey_data(df, "NHL Edge Goalie 5v5 Top 10", Sys.time())
}
