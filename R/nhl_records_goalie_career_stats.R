#' @title **NHL Records - Goalie Career Stats**
#' @description Returns career goalie statistics from the NHL Records API
#'   (`https://records.nhl.com/site/api/goalie-career-stats`).
#' @param cayenne_exp Optional Cayenne filter expression string (e.g.
#'   `"playerId=8471679"`).
#' @param limit Optional integer page size.
#' @param start Optional integer pagination offset.
#' @return A `fastRhockey_data` tibble of career goalie stats, or `NULL`
#'   on failure.
#' @keywords NHL Records Goalie Career Stats
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_goalie_career_stats(limit = 5))
#' }
nhl_records_goalie_career_stats <- function(
    cayenne_exp = NULL,
    limit = NULL,
    start = NULL
) {
    raw <- .nhl_records_api(
        resource = "goalie-career-stats",
        cayenne_exp = cayenne_exp,
        limit = limit,
        start = start
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No goalie career stats data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(
        df,
        "NHL Records Goalie Career Stats",
        Sys.time()
    )
}
