#' @title **NHL Schedule Calendar**
#' @description Returns the schedule calendar showing which dates have games.
#' @param date Character date in "YYYY-MM-DD" format. If NULL, returns current.
#' @return Returns a list with calendar data.
#' @keywords NHL Schedule Calendar
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_schedule_calendar())
#' }
nhl_schedule_calendar <- function(date = NULL) {
    if (is.null(date)) {
        url <- "https://api-web.nhle.com/v1/schedule-calendar/now"
    } else {
        url <- glue::glue(
            "https://api-web.nhle.com/v1/schedule-calendar/{date}"
        )
    }

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            return(raw)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching schedule calendar: {e$message}"
            ))
            return(NULL)
        }
    )
}
