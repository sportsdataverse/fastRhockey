#' @title **NHL Stats API — Content Module**
#' @description Returns an NHL.com CMS content module from the Stats REST
#'   API (`https://api.nhle.com/stats/rest/{lang}/content/module/{template_key}`).
#' @param template_key Character (required). The content module template
#'   key (NHL.com internal identifier).
#' @param lang Character language code. Default `"en"`.
#' @return A `fastRhockey_data` tibble of content module rows, or `NULL` on
#'   failure.
#' @keywords NHL Stats Content Module
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_content_module(template_key = "example"))
#' }
nhl_stats_content_module <- function(template_key, lang = "en") {
    url <- glue::glue(
        "https://api.nhle.com/stats/rest/{lang}/content/module/{template_key}"
    )

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            # The CMS may return either an empty `data` element or a
            # named list without column names (which crashes clean_names).
            # Guard for both before attempting to tidy.
            if (
                is.null(raw$data) ||
                    (is.data.frame(raw$data) && nrow(raw$data) == 0) ||
                    (is.list(raw$data) && !is.data.frame(raw$data) &&
                        length(raw$data) == 0) ||
                    (is.data.frame(raw$data) && is.null(names(raw$data)))
            ) {
                message(glue::glue(
                    "{Sys.time()}: No content module data for '{template_key}'"
                ))
                return(NULL)
            }

            df <- raw$data %>%
                janitor::clean_names() %>%
                make_fastRhockey_data(
                    "NHL Stats Content Module",
                    Sys.time()
                )

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching content module: {e$message}"
            ))
            return(NULL)
        }
    )
}
