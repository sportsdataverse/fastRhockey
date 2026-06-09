#' @title **NHL TV Schedule**
#' @description Returns the TV schedule for NHL games on a given date.
#' @param date Character date in "YYYY-MM-DD" format. If NULL, returns current.
#' @return A named list of data frames: `broadcasts`.
#'
#'    **broadcasts**
#'
#'    |col_name           |types     |description                                  |
#'    |:------------------|:---------|:--------------------------------------------|
#'    |startTime          |character |Broadcast start time (UTC).                  |
#'    |endTime            |character |Broadcast end time (UTC).                    |
#'    |durationSeconds    |integer   |Broadcast duration in seconds.               |
#'    |title              |character |Broadcast title.                             |
#'    |description        |character |Broadcast description.                       |
#'    |houseNumber        |character |Internal broadcast house number identifier.  |
#'    |broadcastType      |character |Type of broadcast.                           |
#'    |broadcastStatus    |character |Broadcast status.                            |
#'    |broadcastImageUrl  |character |URL to the broadcast image.                  |
#' @keywords NHL TV Schedule
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_tv_schedule())
#' }
nhl_tv_schedule <- function(date = NULL) {
    if (is.null(date)) {
        url <- "https://api-web.nhle.com/v1/network/tv-schedule/now"
    } else {
        url <- glue::glue(
            "https://api-web.nhle.com/v1/network/tv-schedule/{date}"
        )
    }

    tryCatch(
        expr = {
            res <- .retry_request(url)
            check_status(res)

            resp_text <- .resp_text(res)
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            return(raw)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching TV schedule: {e$message}"
            ))
            return(NULL)
        }
    )
}
