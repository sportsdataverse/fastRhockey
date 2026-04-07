#' @title **NHL Teams Info**
#' @description Returns NHL team information for a given team abbreviation.
#' Uses the new NHL API via \code{\link{nhl_teams}}.
#'
#' **Breaking change:** The old `team_id` (integer) parameter has been replaced
#' by `team_abbr` (3-letter string, e.g., "TBL") because the new NHL API no
#' longer exposes numeric team IDs as a primary identifier.
#'
#' @param team_abbr Three-letter team abbreviation (e.g., "TBL", "TOR", "SEA")
#' @return Returns a data frame with team information filtered to the given team.
#' @keywords NHL Teams Info
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_teams_info(team_abbr = "TBL"))
#' }
nhl_teams_info <- function(team_abbr) {
  tryCatch(
    expr = {
      teams <- nhl_teams()

      if (is.null(teams) || nrow(teams) == 0) {
        message(glue::glue("{Sys.time()}: No team data available"))
        return(NULL)
      }

      team_df <- teams[teams$team_abbr == team_abbr, ]

      if (nrow(team_df) == 0) {
        message(glue::glue(
          "{Sys.time()}: No team found for abbreviation '{team_abbr}'. ",
          "Use nhl_teams() to see valid abbreviations."
        ))
        return(NULL)
      }

      team_df <- team_df %>%
        make_fastRhockey_data("NHL Teams Information from NHL.com", Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no team info data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(team_df)
}
