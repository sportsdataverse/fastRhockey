#' @title **NHL Draft Tracker**
#' @description Returns the live (real-time) NHL draft tracker picks from the
#' NHL web service endpoint `draft-tracker/picks/now`.
#'
#' This is distinct from [`nhl_draft()`], which hits `draft/picks/now` and
#' returns the static draft board. The draft tracker is only populated during
#' an active NHL draft window and will return `NULL` (or an empty payload)
#' outside of that window.
#' @return Returns a data frame of live draft picks, or `NULL` if no draft is
#'   currently active.
#' @keywords NHL Draft Tracker
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_draft_tracker())
#' }
nhl_draft_tracker <- function() {
    url <- "https://api-web.nhle.com/v1/draft-tracker/picks/now"

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            picks <- NULL
            if (is.data.frame(raw)) {
                picks <- raw
            } else if (is.list(raw) && !is.null(raw[["picks"]])) {
                picks <- raw[["picks"]]
            }

            if (is.null(picks) ||
                (is.data.frame(picks) && nrow(picks) == 0) ||
                length(picks) == 0) {
                message(glue::glue(
                    "{Sys.time()}: No active NHL draft tracker data available!"
                ))
                return(NULL)
            }

            picks_df <- as.data.frame(picks) %>%
                janitor::clean_names() %>%
                make_fastRhockey_data(
                    "NHL Draft Tracker data from NHL.com",
                    Sys.time()
                )
            return(picks_df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching NHL draft tracker: {e$message}"
            ))
            return(NULL)
        }
    )
}
