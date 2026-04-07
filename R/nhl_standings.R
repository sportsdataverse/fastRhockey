#' @title **NHL Standings**
#' @description Returns current or historical NHL standings.
#' Uses the new NHL API (`api-web.nhle.com`).
#' @param date Character date in "YYYY-MM-DD" format. If NULL, returns
#'   current standings.
#' @return Returns a data frame with standings information.
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
