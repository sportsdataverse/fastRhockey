#' @title **NHL Conference Info**
#' @description Returns teams belonging to a given conference, derived from standings data.
#'
#' The original NHL Stats API conferences endpoint is no longer available.
#' This function now extracts conference information from the standings endpoint
#' at `api-web.nhle.com`.
#'
#' @param conference_name Character. Conference name (e.g. "Eastern" or "Western").
#' @param date Character date in "YYYY-MM-DD" format. If NULL, returns
#'   current conference info.
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
