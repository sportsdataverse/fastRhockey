#' @title **NHL Postal Lookup**
#' @description Returns broadcast region / geo-lookup information for a given
#' postal (or ZIP) code from the NHL web service endpoint
#' `postal-lookup/{postalCode}`. Used internally by NHL.com to determine
#' local broadcast rights.
#' @param postal_code Character. Postal or ZIP code to look up (e.g.,
#'   `"10001"` for New York City).
#' @return Returns a list with broadcast region information for the given
#'   postal code.
#' @keywords NHL Postal Lookup
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_postal_lookup(postal_code = "10001"))
#' }
nhl_postal_lookup <- function(postal_code) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/postal-lookup/{postal_code}"
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
                "{Sys.time()}: Error fetching postal lookup for {postal_code}: {e$message}"
            ))
            return(NULL)
        }
    )
}
