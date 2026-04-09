#' @title **NHL Stats API — Ping**
#' @description Health-check endpoint for the NHL Stats REST API
#'   (`https://api.nhle.com/stats/rest/ping`). This endpoint is non-tabular
#'   and is not language-scoped: the raw parsed payload is returned as-is
#'   rather than wrapped in a `fastRhockey_data` tibble.
#' @return A parsed list with the health-check payload, or `NULL` on
#'   failure.
#' @keywords NHL Stats Ping
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_ping())
#' }
nhl_stats_ping <- function() {
    url <- "https://api.nhle.com/stats/rest/ping"

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- tryCatch(
                jsonlite::fromJSON(resp_text, flatten = TRUE),
                error = function(e) resp_text
            )

            return(raw)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching ping: {e$message}"
            ))
            return(NULL)
        }
    )
}
