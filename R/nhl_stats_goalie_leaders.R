#' @title **NHL Stats API — Goalie Leaders**
#' @description Returns a goalie leaderboard for a given statistic attribute
#'   from the NHL Stats REST API
#'   (`https://api.nhle.com/stats/rest/{lang}/leaders/goalies/{attribute}`).
#' @param attribute Character (required). The stat attribute to rank by.
#'   Known valid values: `"savePctg"`, `"gaa"`, `"shutouts"`. Note that
#'   `"wins"`, `"saves"`, `"shotsAgainst"`, and `"goalsAgainst"` return
#'   500 errors. The endpoint also does **not** accept `start` / `limit`
#'   query parameters — passing them produces a 500.
#' @param lang Character language code. Default `"en"`.
#' @param cayenne_exp Optional Cayenne filter expression string passed via
#'   the `cayenneExp` query parameter (e.g., `"seasonId=20242025"`).
#' @return A `fastRhockey_data` tibble of goalie leaders, or `NULL` on
#'   failure.
#' @keywords NHL Stats Goalie Leaders
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_goalie_leaders(attribute = "savePctg"))
#' }
nhl_stats_goalie_leaders <- function(
    attribute,
    lang = "en",
    cayenne_exp = NULL
) {
    url <- glue::glue(
        "https://api.nhle.com/stats/rest/{lang}/leaders/goalies/{attribute}"
    )
    if (!is.null(cayenne_exp)) {
        url <- paste0(
            url,
            "?cayenneExp=",
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
                message(glue::glue(
                    "{Sys.time()}: No goalie leaders data for '{attribute}'"
                ))
                return(NULL)
            }

            df <- raw$data %>%
                janitor::clean_names() %>%
                make_fastRhockey_data(
                    "NHL Stats Goalie Leaders",
                    Sys.time()
                )

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching goalie leaders: {e$message}"
            ))
            return(NULL)
        }
    )
}
