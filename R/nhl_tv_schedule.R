#' @title **NHL TV Schedule**
#' @description Returns the TV schedule for NHL games on a given date.
#' @param date Character date in "YYYY-MM-DD" format. If NULL, returns current.
#' @return Returns a list with TV schedule data.
#' @keywords NHL TV Schedule
#' @importFrom httr RETRY content
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
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
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
