#' @title **NHL Divisions**
#' @description Returns information on NHL divisions derived from standings data.
#'
#' The original NHL Stats API divisions endpoint is no longer available.
#' This function now extracts division information from the standings endpoint
#' at \code{api-web.nhle.com}.
#'
#' @param date Character date in "YYYY-MM-DD" format. If NULL, returns
#'   current divisions.
#' @return Returns a data frame with columns:
#'   \itemize{
#'     \item \code{division_name} - division name (e.g. "Atlantic", "Metropolitan")
#'     \item \code{division_abbrev} - division abbreviation
#'     \item \code{conference_name} - parent conference name
#'   }
#' @keywords NHL Divisions
#' @importFrom dplyr distinct select
#' @export
#' @examples
#' \donttest{
#'    try(nhl_divisions())
#' }
nhl_divisions <- function(date = NULL) {
  tryCatch(
    expr = {
      standings <- nhl_standings(date = date)
      if (is.null(standings) || nrow(standings) == 0) {
        message(glue::glue("{Sys.time()}: No standings data available to extract divisions!"))
        return(NULL)
      }
      divisions_df <- standings %>%
        dplyr::distinct(.data$division_name, .data$division_abbrev, .data$conference_name) %>%
        as.data.frame() %>%
        make_fastRhockey_data("NHL Divisions from NHL.com", Sys.time())
      return(divisions_df)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no division data available!"))
      return(NULL)
    }
  )
}
