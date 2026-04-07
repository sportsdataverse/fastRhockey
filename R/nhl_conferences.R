#' @title **NHL Conferences**
#' @description Returns a table of current NHL conferences derived from standings data.
#'
#' The original NHL Stats API conferences endpoint is no longer available.
#' This function now extracts conference information from the standings endpoint
#' at \code{api-web.nhle.com}.
#'
#' @param date Character date in "YYYY-MM-DD" format. If NULL, returns
#'   current conferences.
#' @return Returns a data frame with columns:
#'   \itemize{
#'     \item \code{conference_name} - conference name (e.g. "Eastern", "Western")
#'   }
#' @keywords NHL Conferences
#' @importFrom dplyr distinct select
#' @export
#' @examples
#' \donttest{
#'   try(nhl_conferences())
#' }
nhl_conferences <- function(date = NULL) {
  tryCatch(
    expr = {
      standings <- nhl_standings(date = date)
      if (is.null(standings) || nrow(standings) == 0) {
        message(glue::glue("{Sys.time()}: No standings data available to extract conferences!"))
        return(NULL)
      }
      conferences_df <- standings %>%
        dplyr::distinct(conference_name) %>%
        as.data.frame() %>%
        make_fastRhockey_data("NHL Conferences from NHL.com", Sys.time())
      return(conferences_df)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no conferences data available!"))
      return(NULL)
    }
  )
}
