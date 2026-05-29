#' @title **NHL Stats API — Glossary**
#' @description Returns the glossary of stat definitions from the NHL Stats
#'   REST API (`https://api.nhle.com/stats/rest/{lang}/glossary`). Useful to
#'   look up what a given abbreviation or statistic (e.g., `PIM`) means.
#' @param lang Character language code. Default `"en"`.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                |types     |description                          |
#'    |:-----------------------|:---------|:------------------------------------|
#'    |id                      |integer   |Unique glossary entry identifier.    |
#'    |abbreviation            |character |Statistic abbreviation.              |
#'    |definition              |character |Definition of the statistic.         |
#'    |first_season_for_stat   |integer   |First season the stat was tracked.   |
#'    |full_name               |character |Full name of the statistic.          |
#'    |language_code           |character |Language code of the entry.          |
#'    |last_updated            |character |Timestamp the entry was last updated.|
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
