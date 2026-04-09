#' @title **NHL Records - Goalie Shutout Streaks**
#' @description Returns goalie shutout streak records from the NHL Records
#'   API (`https://records.nhl.com/site/api/goalie-shutout-streak`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @param limit Optional integer page size.
#' @param start Optional integer pagination offset.
#' @return A `fastRhockey_data` tibble of goalie shutout streaks, or `NULL`
#'   on failure.
#' @keywords NHL Records Goalie Shutout Streak
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_goalie_shutout_streak(limit = 5))
#' }
nhl_records_goalie_shutout_streak <- function(
    cayenne_exp = NULL,
    limit = NULL,
    start = NULL
) {
    raw <- .nhl_records_api(
        resource = "goalie-shutout-streak",
        cayenne_exp = cayenne_exp,
        limit = limit,
        start = start
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No goalie shutout streak data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(
        df,
        "NHL Records Goalie Shutout Streak",
        Sys.time()
    )
}
