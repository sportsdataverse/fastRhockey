#' @title **NHL Draft Prospects**
#' @description Returns current draft prospect rankings.
#'
#' Uses the new NHL API endpoint at
#' \code{api-web.nhle.com/v1/draft/rankings/now}.
#'
#' @return Returns a data frame of draft prospect rankings.
#' @keywords NHL Draft Prospects
#' @importFrom jsonlite read_json
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'    try(nhl_draft_prospects())
#' }
nhl_draft_prospects <- function() {
  url <- "https://api-web.nhle.com/v1/draft/rankings/now"

  tryCatch(
    expr = {
      raw <- jsonlite::read_json(url, simplifyVector = TRUE)
      # The response may have different structures depending on the API version
      # Try common keys
      rankings <- NULL
      for (key in c("rankings", "prospects", "data")) {
        if (!is.null(raw[[key]])) {
          rankings <- raw[[key]]
          break
        }
      }
      if (is.null(rankings)) {
        # If no known key, try to use the raw response if it's a data frame-like list
        if (is.data.frame(raw)) {
          rankings <- raw
        } else if (is.list(raw) && length(raw) > 0) {
          # Try to flatten the first list element
          rankings <- tryCatch(
            as.data.frame(raw, stringsAsFactors = FALSE),
            error = function(e2) NULL
          )
        }
      }
      if (is.null(rankings) || (is.data.frame(rankings) && nrow(rankings) == 0)) {
        message(glue::glue("{Sys.time()}: No draft prospects data available!"))
        return(NULL)
      }
      draft_prospects_df <- as.data.frame(rankings)
      draft_prospects_df <- draft_prospects_df %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Draft Prospects data from NHL.com", Sys.time())
      return(draft_prospects_df)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no draft prospects data available!"))
      return(NULL)
    }
  )
}
