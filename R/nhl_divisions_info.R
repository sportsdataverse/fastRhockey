#' @title **NHL Divisions Info**
#' @description Returns teams belonging to a given division, derived from standings data.
#'
#' The original NHL Stats API divisions endpoint is no longer available.
#' This function now extracts division information from the standings endpoint
#' at `api-web.nhle.com`.
#'
#' @param division_name Character. Division name (e.g. "Atlantic", "Metropolitan",
#'   "Central", "Pacific").
#' @param date Character date in "YYYY-MM-DD" format. If NULL, returns
#'   current division info.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                |types     |description                                       |
#'    |:-----------------------|:---------|:-------------------------------------------------|
#'    |team_abbr               |character |Team abbreviation.                                |
#'    |team_name               |character |Team name.                                        |
#'    |team_common_name        |character |Team common (nickname) name.                      |
#'    |team_logo               |character |URL to the team logo image.                       |
#'    |conference_name         |character |Conference name.                                  |
#'    |division_abbrev         |character |Division abbreviation.                            |
#'    |division_name           |character |Division name.                                    |
#'    |place_name              |character |Team place (city) name.                           |
#'    |conference_sequence     |integer   |Team's seeding position within the conference.    |
#'    |division_sequence       |integer   |Team's seeding position within the division.      |
#'    |league_sequence         |integer   |Team's seeding position within the league.        |
#'    |wildcard_sequence       |integer   |Team's wild card seeding position.                |
#'    |games_played            |integer   |Games played.                                     |
#'    |wins                    |integer   |Wins.                                             |
#'    |losses                  |integer   |Losses.                                           |
#'    |ot_losses               |integer   |Overtime losses.                                  |
#'    |points                  |integer   |Standings points.                                 |
#'    |point_pctg              |numeric   |Points percentage.                                |
#'    |regulation_wins         |integer   |Wins in regulation.                               |
#'    |regulation_plus_ot_wins |integer   |Wins in regulation plus overtime.                 |
#'    |goals_for               |integer   |Goals scored.                                     |
#'    |goals_against           |integer   |Goals against.                                    |
#'    |goal_differential       |integer   |Goal differential (goals for minus goals against).|
#'    |home_wins               |integer   |Home wins.                                        |
#'    |home_losses             |integer   |Home losses.                                      |
#'    |home_ot_losses          |integer   |Home overtime losses.                             |
#'    |road_wins               |integer   |Road wins.                                        |
#'    |road_losses             |integer   |Road losses.                                      |
#'    |road_ot_losses          |integer   |Road overtime losses.                             |
#'    |l10_wins                |integer   |Wins in the last ten games.                       |
#'    |l10_losses              |integer   |Losses in the last ten games.                     |
#'    |l10_ot_losses           |integer   |Overtime losses in the last ten games.            |
#'    |streak_code             |character |Current streak type code (W/L/OT).                |
#'    |streak_count            |integer   |Length of the current streak.                     |
#'    |shootout_wins           |integer   |Shootout wins.                                    |
#'    |shootout_losses         |integer   |Shootout losses.                                  |
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
