#' @title **NHL Scoreboard**
#' @description Returns the current league-wide scoreboard with today's games.
#' @return Returns a list with scoreboard data (focusedDate, games by date).
#' @keywords NHL Scoreboard
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \donttest{
#'   try(nhl_scoreboard())
#' }
nhl_scoreboard <- function() {
    url <- "https://api-web.nhle.com/v1/scoreboard/now"

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
