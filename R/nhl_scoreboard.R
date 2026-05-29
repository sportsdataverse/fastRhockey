#' @title **NHL Scoreboard**
#' @description Returns the current league-wide scoreboard with today's games,
#' or — when `date` is supplied — the scoreboard for a specific date.
#' @param date Optional character string in `"YYYY-MM-DD"` format. When
#'   `NULL` (default) the function hits `scoreboard/now`; when a date is
#'   provided it hits `scoreboard/{date}`.
#' @return A named list of data frames: `gamesByDate`.
#'
#'    **gamesByDate**
#'
#'    |col_name |types     |description                                     |
#'    |:--------|:---------|:-----------------------------------------------|
#'    |date     |character |Date for which the games are listed.            |
#'    |games    |list      |Nested list of games scheduled on the date.     |
#' @keywords NHL Scoreboard
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_scoreboard())
#'   try(nhl_scoreboard(date = "2024-01-15"))
#' }
nhl_scoreboard <- function(date = NULL) {
    if (is.null(date)) {
        url <- "https://api-web.nhle.com/v1/scoreboard/now"
    } else {
        url <- glue::glue(
            "https://api-web.nhle.com/v1/scoreboard/{date}"
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
                "{Sys.time()}: Error fetching scoreboard: {e$message}"
            ))
            return(NULL)
        }
    )
}
