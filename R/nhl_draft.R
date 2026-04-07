#' @title **NHL Draft**
#' @description Returns information on the most recent NHL draft picks.
#'
#' Uses the new NHL API endpoint at \code{api-web.nhle.com/v1/draft/picks/now}.
#'
#' @return Returns a data frame of draft picks.
#' @keywords NHL Draft
#' @importFrom jsonlite read_json
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'    try(nhl_draft())
#' }
nhl_draft <- function() {
  url <- "https://api-web.nhle.com/v1/draft/picks/now"

  tryCatch(
    expr = {
      raw <- jsonlite::read_json(url, simplifyVector = TRUE)
      picks <- raw[["picks"]]
      if (is.null(picks) || length(picks) == 0) {
        # try top-level if "picks" key doesn't exist
        if (is.data.frame(raw)) {
          picks <- raw
        } else {
          message(glue::glue("{Sys.time()}: No draft picks data available!"))
          return(NULL)
        }
      }
      draft_df <- as.data.frame(picks)
      draft_df <- draft_df %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Draft Data from NHL.com", Sys.time())
      return(draft_df)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no draft data available!"))
      return(NULL)
    }
  )
}
