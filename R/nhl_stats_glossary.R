#' @title **NHL Stats API — Glossary**
#' @description Returns the glossary of stat definitions from the NHL Stats
#'   REST API (`https://api.nhle.com/stats/rest/{lang}/glossary`). Useful to
#'   look up what a given abbreviation or statistic (e.g., `PIM`) means.
#' @param lang Character language code. Default `"en"`.
#' @return A `fastRhockey_data` tibble of glossary entries, or `NULL` on
#'   failure.
#' @keywords NHL Stats Glossary
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_glossary())
#' }
nhl_stats_glossary <- function(lang = "en") {
    url <- glue::glue(
        "https://api.nhle.com/stats/rest/{lang}/glossary"
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
                message(glue::glue("{Sys.time()}: No glossary data"))
                return(NULL)
            }

            df <- raw$data %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Stats Glossary", Sys.time())

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching glossary: {e$message}"
            ))
            return(NULL)
        }
    )
}
