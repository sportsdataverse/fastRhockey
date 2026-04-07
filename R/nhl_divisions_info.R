#' @title **NHL Divisions Info**
#' @description Returns teams belonging to a given division, derived from standings data.
#'
#' The original NHL Stats API divisions endpoint is no longer available.
#' This function now extracts division information from the standings endpoint
#' at \code{api-web.nhle.com}.
#'
#' @param division_name Character. Division name (e.g. "Atlantic", "Metropolitan",
#'   "Central", "Pacific").
#' @param date Character date in "YYYY-MM-DD" format. If NULL, returns
#'   current division info.
#' @return Returns a data frame of teams in the specified division with columns
#'   from \code{\link{nhl_standings}} filtered by division.
#' @keywords NHL Division Info
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_divisions_info(division_name = "Atlantic"))
#' }
nhl_divisions_info <- function(division_name, date = NULL) {
  tryCatch(
    expr = {
      standings <- nhl_standings(date = date)
      if (is.null(standings) || nrow(standings) == 0) {
        message(glue::glue("{Sys.time()}: No standings data available!"))
        return(NULL)
      }
      div_df <- standings %>%
        dplyr::filter(.data$division_name == !!division_name)
      if (nrow(div_df) == 0) {
        message(glue::glue(
          "{Sys.time()}: No data found for division '{division_name}'. ",
          "Valid values: {paste(unique(standings$division_name), collapse = ', ')}"
        ))
        return(NULL)
      }
      div_df <- div_df %>%
        as.data.frame() %>%
        make_fastRhockey_data("NHL Division Information from NHL.com", Sys.time())
      return(div_df)
    },
    error = function(e) {
      message(glue::glue(
        "{Sys.time()}: Invalid arguments or no division info data for '{division_name}' available!"
      ))
      return(NULL)
    }
  )
}
