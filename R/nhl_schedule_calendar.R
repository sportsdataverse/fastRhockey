#' @title **NHL Schedule Calendar**
#' @description Returns the schedule calendar showing which dates have games.
#' @param date Character date in "YYYY-MM-DD" format. If NULL, returns current.
#' @return A named list of data frames: `teams`.
#'
#'    **teams**
#'
#'    |col_name                         |types     |description                            |
#'    |:--------------------------------|:---------|:--------------------------------------|
#'    |id                               |integer   |Unique team identifier.                |
#'    |seasonId                         |integer   |Season identifier (8-digit).           |
#'    |abbrev                           |character |Team abbreviation.                     |
#'    |logo                             |character |URL of the team logo.                  |
#'    |darkLogo                         |character |URL of the team dark-mode logo.        |
#'    |french                           |logical   |Whether the team uses French naming.   |
#'    |commonName.default               |character |Team common name (default language).   |
#'    |commonName.fr                    |character |Team common name (French).             |
#'    |name.default                     |character |Team name (default language).          |
#'    |name.fr                          |character |Team name (French).                    |
#'    |placeNameWithPreposition.default |character |Place name with preposition (default). |
#'    |placeNameWithPreposition.fr      |character |Place name with preposition (French).  |
#'    |placeName.default                |character |Place name (default language).         |
#'    |placeName.fr                     |character |Place name (French).                   |
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
