#' @title **NHL Records - Player Career Stats**
#' @description Returns career player stats from the NHL Records API
#'   (`https://records.nhl.com/site/api/player-stats`). Supports Cayenne
#'   filtering and pagination.
#' @param cayenne_exp Optional Cayenne filter expression string (e.g.
#'   `"playerId=8478402"`).
#' @param limit Optional integer page size.
#' @param start Optional integer pagination offset.
#' @return A `fastRhockey_data` tibble of player career stats, or `NULL`
#'   on failure.
#' @keywords NHL Records Player Stats
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_player_stats(cayenne_exp = "playerId=8478402"))
#' }
nhl_records_player_stats <- function(
    cayenne_exp = NULL,
    limit = NULL,
    start = NULL
) {
    raw <- .nhl_records_api(
        resource = "player-stats",
        cayenne_exp = cayenne_exp,
        limit = limit,
        start = start
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No player stats data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Player Stats", Sys.time())
}
