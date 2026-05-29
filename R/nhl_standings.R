#' @title **NHL Standings**
#' @description Returns current or historical NHL standings.
#' Uses the new NHL API (`api-web.nhle.com`).
#' @param date Character date in "YYYY-MM-DD" format. If NULL, returns
#'   current standings.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                |types     |description                |
#'    |:-----------------------|:---------|:--------------------------|
#'    |team_abbr               |character |Team abbreviation.         |
#'    |team_name               |character |Team name.                 |
#'    |team_common_name        |character |Team common name.          |
#'    |team_logo               |character |URL to the team logo image. |
#'    |conference_name         |character |Conference name.           |
#'    |division_abbrev         |character |Division abbreviation.     |
#'    |division_name           |character |Division name.             |
#'    |place_name              |character |Place (city) name.         |
#'    |conference_sequence     |integer   |Rank within the conference. |
#'    |division_sequence       |integer   |Rank within the division.  |
#'    |league_sequence         |integer   |Rank within the league.    |
#'    |wildcard_sequence       |integer   |Wildcard standing rank.    |
#'    |games_played            |integer   |Games played.              |
#'    |wins                    |integer   |Wins.                      |
#'    |losses                  |integer   |Losses.                    |
#'    |ot_losses               |integer   |Overtime losses.           |
#'    |points                  |integer   |Standings points.          |
#'    |point_pctg              |numeric   |Points percentage.         |
#'    |regulation_wins         |integer   |Regulation wins.           |
#'    |regulation_plus_ot_wins |integer   |Regulation plus overtime wins. |
#'    |goals_for               |integer   |Goals scored for.          |
#'    |goals_against           |integer   |Goals allowed against.     |
#'    |goal_differential       |integer   |Goal differential.         |
#'    |home_wins               |integer   |Home wins.                 |
#'    |home_losses             |integer   |Home losses.               |
#'    |home_ot_losses          |integer   |Home overtime losses.      |
#'    |road_wins               |integer   |Road wins.                 |
#'    |road_losses             |integer   |Road losses.               |
#'    |road_ot_losses          |integer   |Road overtime losses.      |
#'    |l10_wins                |integer   |Wins in last 10 games.     |
#'    |l10_losses              |integer   |Losses in last 10 games.   |
#'    |l10_ot_losses           |integer   |Overtime losses in last 10 games. |
#'    |streak_code             |character |Current streak code (W/L/OT). |
#'    |streak_count            |integer   |Length of current streak.  |
#'    |shootout_wins           |integer   |Shootout wins.             |
#'    |shootout_losses         |integer   |Shootout losses.           |
#' @keywords NHL Standings
#' @importFrom jsonlite read_json
#' @importFrom dplyr tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_standings())
#'   try(nhl_standings(date = "2024-03-01"))
#' }
nhl_standings <- function(date = NULL) {
    if (is.null(date)) {
        url <- "https://api-web.nhle.com/v1/standings/now"
    } else {
        url <- glue::glue("https://api-web.nhle.com/v1/standings/{date}")
    }

    tryCatch(
        expr = {
            raw <- jsonlite::read_json(url, simplifyVector = TRUE)
            standings <- raw[["standings"]]

            if (is.null(standings) || nrow(standings) == 0) {
                message(glue::glue("{Sys.time()}: No standings data found"))
                return(NULL)
            }

            result <- dplyr::tibble(
                team_abbr = standings$teamAbbrev$default,
                team_name = standings$teamName$default,
                team_common_name = standings$teamCommonName$default,
                team_logo = standings$teamLogo,
                conference_name = standings$conferenceName,
                division_abbrev = standings$divisionAbbrev,
                division_name = standings$divisionName,
                place_name = standings$placeName$default,
                conference_sequence = standings$conferenceSequence,
                division_sequence = standings$divisionSequence,
                league_sequence = standings$leagueSequence,
                wildcard_sequence = standings$wildcardSequence,
                games_played = standings$gamesPlayed,
                wins = standings$wins,
                losses = standings$losses,
                ot_losses = standings$otLosses,
                points = standings$points,
                point_pctg = standings$pointPctg,
                regulation_wins = standings$regulationWins,
                regulation_plus_ot_wins = standings$regulationPlusOtWins,
                goals_for = standings$goalFor,
                goals_against = standings$goalAgainst,
                goal_differential = standings$goalDifferential,
                home_wins = standings$homeWins,
                home_losses = standings$homeLosses,
                home_ot_losses = standings$homeOtLosses,
                road_wins = standings$roadWins,
                road_losses = standings$roadLosses,
                road_ot_losses = standings$roadOtLosses,
                l10_wins = standings$l10Wins,
                l10_losses = standings$l10Losses,
                l10_ot_losses = standings$l10OtLosses,
                streak_code = standings$streakCode,
                streak_count = standings$streakCount,
                shootout_wins = standings$shootoutWins,
                shootout_losses = standings$shootoutLosses
            )

            result <- make_fastRhockey_data(result, "NHL Standings", Sys.time())
            return(result)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching standings: {e$message}"
            ))
            return(NULL)
        }
    )
}
