#' @title **NHL Stats API — Config**
#' @description Returns the Stats REST API configuration payload
#'   (`https://api.nhle.com/stats/rest/{lang}/config`). This endpoint is
#'   non-tabular: the raw parsed list is returned as-is rather than wrapped
#'   in a `fastRhockey_data` tibble.
#' @param lang Character language code. Default `"en"`.
#' @return A parsed list of configuration values, or `NULL` on failure.
#' @keywords NHL Stats Config
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_config())
#' }
nhl_stats_config <- function(lang = "en") {
    url <- glue::glue(
        "https://api.nhle.com/stats/rest/{lang}/config"
    )

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
                "{Sys.time()}: Error fetching config: {e$message}"
            ))
            return(NULL)
        }
    )
}
