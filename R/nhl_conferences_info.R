#' @title **NHL Conference Info**
#' @description Returns teams belonging to a given conference, derived from standings data.
#'
#' The original NHL Stats API conferences endpoint is no longer available.
#' This function now extracts conference information from the standings endpoint
#' at \code{api-web.nhle.com}.
#'
#' @param conference_name Character. Conference name (e.g. "Eastern" or "Western").
#' @param date Character date in "YYYY-MM-DD" format. If NULL, returns
#'   current conference info.
#' @return Returns a data frame of teams in the specified conference with columns
#'   from \code{\link{nhl_standings}} filtered by conference.
#' @keywords NHL Conferences Info
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_conferences_info(conference_name = "Eastern"))
#' }
nhl_conferences_info <- function(conference_name, date = NULL) {
  tryCatch(
    expr = {
      standings <- nhl_standings(date = date)
      if (is.null(standings) || nrow(standings) == 0) {
        message(glue::glue("{Sys.time()}: No standings data available!"))
        return(NULL)
      }
      conf_df <- standings %>%
        dplyr::filter(.data$conference_name == !!conference_name)
      if (nrow(conf_df) == 0) {
        message(glue::glue(
          "{Sys.time()}: No data found for conference '{conference_name}'. ",
          "Valid values: {paste(unique(standings$conference_name), collapse = ', ')}"
        ))
        return(NULL)
      }
      conf_df <- conf_df %>%
        as.data.frame() %>%
        make_fastRhockey_data("NHL Conference Information from NHL.com", Sys.time())
      return(conf_df)
    },
    error = function(e) {
      message(glue::glue(
        "{Sys.time()}: Invalid arguments or no conference info data for '{conference_name}' available!"
      ))
      return(NULL)
    }
  )
}
