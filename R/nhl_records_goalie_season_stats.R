#' @title **NHL Records - Goalie Season Stats**
#' @description Returns season-by-season goalie statistics from the NHL
#'   Records API
#'   (`https://records.nhl.com/site/api/goalie-season-stats`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @param limit Optional integer page size.
#' @param start Optional integer pagination offset.
#' @return A `fastRhockey_data` tibble of goalie season stats, or `NULL`
#'   on failure.
#' @keywords NHL Records Goalie Season Stats
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_goalie_season_stats(limit = 5))
#' }
nhl_records_goalie_season_stats <- function(
    cayenne_exp = NULL,
    limit = NULL,
    start = NULL
) {
    raw <- .nhl_records_api(
        resource = "goalie-season-stats",
        cayenne_exp = cayenne_exp,
        limit = limit,
        start = start
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No goalie season stats data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(
        df,
        "NHL Records Goalie Season Stats",
        Sys.time()
    )
}
