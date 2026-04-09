#' @title **NHL Stats API — Country Lookup**
#' @description Returns the country lookup list from the NHL Stats REST API
#'   (`https://api.nhle.com/stats/rest/{lang}/country`). Used for player
#'   nationality mapping.
#' @param lang Character language code. Default `"en"`.
#' @return A `fastRhockey_data` tibble of countries, or `NULL` on failure.
#' @keywords NHL Stats Country
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_country())
#' }
nhl_stats_country <- function(lang = "en") {
    url <- glue::glue(
        "https://api.nhle.com/stats/rest/{lang}/country"
    )

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            if (
                is.null(raw$data) ||
                    (is.data.frame(raw$data) && nrow(raw$data) == 0)
            ) {
                message(glue::glue("{Sys.time()}: No country data"))
                return(NULL)
            }

            df <- raw$data %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Stats Country", Sys.time())

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching country: {e$message}"
            ))
            return(NULL)
        }
    )
}
