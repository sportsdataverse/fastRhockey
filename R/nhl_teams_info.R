#' @title **NHL Teams Info**
#' @description Returns NHL team information for a given team abbreviation.
#' Uses the new NHL API via [nhl_teams].
#'
#' **Breaking change:** The old `team_id` (integer) parameter has been replaced
#' by `team_abbr` (3-letter string, e.g., "TBL") because the new NHL API no
#' longer exposes numeric team IDs as a primary identifier.
#'
#' @param team_abbr Three-letter team abbreviation (e.g., "TBL", "TOR", "SEA")
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name          |types     |description                                  |
#'    |:-----------------|:---------|:--------------------------------------------|
#'    |team_abbr         |character |Team abbreviation.                           |
#'    |team_name         |character |Team name.                                   |
#'    |team_common_name  |character |Team common name.                            |
#'    |team_logo         |character |URL to the team logo image.                  |
#'    |conference_abbr   |character |Conference abbreviation.                     |
#'    |conference_name   |character |Conference name.                             |
#'    |division_abbr     |character |Division abbreviation.                       |
#'    |division_name     |character |Division name.                               |
#'    |place_name        |character |Team place/city name.                        |
#'    |games_played      |integer   |Games played.                                |
#'    |wins              |integer   |Wins.                                        |
#'    |losses            |integer   |Losses.                                      |
#'    |ot_losses         |integer   |Overtime losses.                             |
#'    |points            |integer   |Total points.                                |
#'    |point_pctg        |numeric   |Points percentage.                           |
#'    |goals_for         |integer   |Goals for.                                   |
#'    |goals_against     |integer   |Goals against.                               |
#'    |goal_differential |integer   |Goal differential.                           |
#'    |streak_code       |character |Current streak code (W/L/OT).                |
#'    |streak_count      |integer   |Length of the current streak.                |
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

      team_df %>%
        make_fastRhockey_data("NHL Teams Information from NHL.com", Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no team info data available!"))
      return(NULL)
    },
    warning = function(w) {
    },
    finally = {
    }
  )
}
