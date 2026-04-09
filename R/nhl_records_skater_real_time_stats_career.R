#' @title **NHL Records - Skater Real-Time Stats Career**
#' @description Returns career skater real-time stats (hits, giveaways,
#'   takeaways, blocks, faceoffs, time on ice) from the NHL Records API
#'   (`https://records.nhl.com/site/api/skater-real-time-stats-career`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @param limit Optional integer page size.
#' @param start Optional integer pagination offset.
#' @return A `fastRhockey_data` tibble of skater real-time career stats,
#'   or `NULL` on failure.
#' @keywords NHL Records Skater Real Time Career
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_skater_real_time_stats_career(limit = 5))
#' }
nhl_records_skater_real_time_stats_career <- function(
    cayenne_exp = NULL,
    limit = NULL,
    start = NULL
) {
    raw <- .nhl_records_api(
        resource = "skater-real-time-stats-career",
        cayenne_exp = cayenne_exp,
        limit = limit,
        start = start
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf(
            "%s: No skater real-time career stats data",
            Sys.time()
        ))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(
        df,
        "NHL Records Skater Real-Time Stats Career",
        Sys.time()
    )
}
