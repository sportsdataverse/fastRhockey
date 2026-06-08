#' @title **NHL Postal Lookup**
#' @description Returns broadcast region / geo-lookup information for a given
#' postal (or ZIP) code from the NHL web service endpoint
#' `postal-lookup/{postalCode}`. Used internally by NHL.com to determine
#' local broadcast rights.
#' @param postal_code Character. Postal or ZIP code to look up (e.g.,
#'   `"10001"` for New York City).
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name         |types     |description                                |
#'    |:----------------|:---------|:------------------------------------------|
#'    |stateProvince    |character |State or province for the postal code.     |
#'    |networkType      |character |Broadcast network type for the region.     |
#'    |county           |character |County for the postal code.                |
#'    |postalCode       |character |Postal or ZIP code looked up.              |
#'    |country          |character |Country for the postal code.               |
#'    |city             |character |City for the postal code.                  |
#'    |teamName.default |character |Local broadcast team name (default).       |
#'    |teamName.fr      |character |Local broadcast team name (French).        |
#' @keywords NHL Postal Lookup
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
            res <- .retry_request(url)
            check_status(res)

            resp_text <- .resp_text(res)
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
