#' @title **NHL Stats API — Franchise Listing**
#' @description Returns the franchise listing from the NHL Stats REST API
#'   (`https://api.nhle.com/stats/rest/{lang}/franchise`). Parallels the
#'   `nhl-api-py` `Teams.franchises()` helper.
#' @param lang Character language code. Default `"en"`.
#' @param limit Integer maximum number of results. Default 100.
#' @param start Integer pagination start index. Default 0.
#' @param cayenne_exp Optional Cayenne filter expression string passed via
#'   the `cayenneExp` query parameter (e.g., `"id=1"`).
#' @return A `fastRhockey_data` tibble of franchises, or `NULL` on failure.
#' @keywords NHL Stats Franchise
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_franchise())
#' }
nhl_stats_franchise <- function(
    lang = "en",
    limit = 100,
    start = 0,
    cayenne_exp = NULL
) {
    base_url <- glue::glue(
        "https://api.nhle.com/stats/rest/{lang}/franchise"
    )
    url <- paste0(base_url, "?start=", start, "&limit=", limit)
    if (!is.null(cayenne_exp)) {
        url <- paste0(
            url,
            "&cayenneExp=",
            utils::URLencode(cayenne_exp, reserved = TRUE)
        )
    }

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
                message(glue::glue("{Sys.time()}: No franchise data"))
                return(NULL)
            }

            df <- raw$data %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Stats Franchise", Sys.time())

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching franchise: {e$message}"
            ))
            return(NULL)
        }
    )
}
