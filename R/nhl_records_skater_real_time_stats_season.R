#' @title **NHL Records - Skater Real-Time Stats by Season**
#' @description Returns season-by-season skater real-time stats (hits,
#'   giveaways, takeaways, blocks, faceoffs, time on ice) from the NHL
#'   Records API
#'   (`https://records.nhl.com/site/api/skater-real-time-stats-season`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @param limit Optional integer page size.
#' @param start Optional integer pagination offset.
#' @return A `fastRhockey_data` tibble of skater real-time season stats,
#'   or `NULL` on failure.
#' @keywords NHL Records Skater Real Time Season
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_skater_real_time_stats_season(limit = 5))
#' }
nhl_records_skater_real_time_stats_season <- function(
    cayenne_exp = NULL,
    limit = NULL,
    start = NULL
) {
    raw <- .nhl_records_api(
        resource = "skater-real-time-stats-season",
        cayenne_exp = cayenne_exp,
        limit = limit,
        start = start
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf(
            "%s: No skater real-time season stats data",
            Sys.time()
        ))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(
        df,
        "NHL Records Skater Real-Time Stats Season",
        Sys.time()
    )
}
