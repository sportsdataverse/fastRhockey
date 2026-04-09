#' @title **NHL Smart Links**
#' @description Returns NHL "smart link" routing metadata from the NHL web
#' service endpoint `smartlinks`. Used by NHL.com's link router to resolve
#' short handles into destination URLs.
#' @param handle Optional character handle. If provided, the lookup is
#'   narrowed to a single smart link via the `handle` query parameter.
#' @return Returns a list with smart link routing data.
#' @keywords NHL Smart Links
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_smartlinks())
#' }
nhl_smartlinks <- function(handle = NULL) {
    url <- "https://api-web.nhle.com/v1/smartlinks"

    tryCatch(
        expr = {
            if (is.null(handle)) {
                res <- httr::RETRY("GET", url)
            } else {
                res <- httr::RETRY(
                    "GET",
                    url,
                    query = list(handle = handle)
                )
            }
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            return(raw)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching NHL smartlinks: {e$message}"
            ))
            return(NULL)
        }
    )
}
