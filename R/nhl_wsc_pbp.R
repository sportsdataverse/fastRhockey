#' @title **NHL WSC Play-by-Play**
#' @description Returns the narrative-format (Web Services Content) play-by-play
#' feed for a given NHL game ID from the NHL web service.
#'
#' This endpoint is distinct from [`nhl_game_pbp()`], which hits the
#' `gamecenter/{id}/play-by-play` endpoint and returns a tabular play-event
#' feed. `nhl_wsc_pbp()` hits `wsc/play-by-play/{gameId}` and returns the
#' narrative-format payload used by NHL.com's story / recap pages (closer in
#' shape to [`nhl_game_story()`]).
#' @param game_id Integer or character game ID (e.g., 2023020001).
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name              |types     |description                                       |
#'    |:---------------------|:---------|:-------------------------------------------------|
#'    |id                    |integer   |Unique play event identifier.                     |
#'    |eventId               |integer   |Event identifier within the game.                 |
#'    |period                |integer   |Period number.                                    |
#'    |timeInPeriod          |character |Elapsed time in the period (MM:SS).               |
#'    |secondsRemaining      |integer   |Seconds remaining in the period.                  |
#'    |situationCode         |character |On-ice situation code.                            |
#'    |typeCode              |integer   |Numeric event type code.                          |
#'    |typeDescKey           |character |Event type description key.                        |
#'    |homeTeamDefendingSide |character |Side of the ice the home team is defending.       |
#'    |sortOrder             |integer   |Sort order of the event.                          |
#'    |utc                   |character |UTC timestamp of the event.                       |
#'    |eventOwnerTeamId      |integer   |Team identifier that owns the event.              |
#'    |losingPlayerId        |integer   |Player identifier who lost the faceoff.           |
#'    |winningPlayerId       |integer   |Player identifier who won the faceoff.            |
#'    |xCoord                |integer   |X coordinate of the event on the ice.             |
#'    |yCoord                |integer   |Y coordinate of the event on the ice.             |
#'    |zoneCode              |character |Zone code where the event occurred.               |
#'    |hittingPlayerId       |integer   |Player identifier delivering the hit.             |
#'    |hitteePlayerId        |integer   |Player identifier receiving the hit.              |
#'    |playerId              |integer   |Player identifier involved in the event.          |
#'    |shotType              |character |Type of shot.                                     |
#'    |shootingPlayerId      |integer   |Player identifier taking the shot.                |
#'    |goalieInNetId         |integer   |Goalie identifier in net for the event.           |
#'    |awaySOG               |integer   |Away team shots on goal at the event.             |
#'    |homeSOG               |integer   |Home team shots on goal at the event.             |
#'    |reason                |character |Reason associated with the event.                 |
#'    |blockingPlayerId      |integer   |Player identifier blocking the shot.              |
#'    |goalModifier          |character |Goal modifier description.                        |
#'    |strength              |character |Strength state of the goal.                       |
#'    |scoringPlayerId       |integer   |Player identifier scoring the goal.               |
#'    |assist1PlayerId       |integer   |Player identifier for the primary assist.         |
#'    |assist2PlayerId       |integer   |Player identifier for the secondary assist.       |
#'    |awayScore             |integer   |Away team score at the event.                     |
#'    |homeScore             |integer   |Home team score at the event.                     |
#'    |strengthCode          |integer   |Numeric strength state code.                      |
#'    |goalCode              |integer   |Numeric goal code.                                |
#'    |scoringPlayerTotal    |integer   |Season goal total for the scorer.                 |
#'    |assist1PlayerTotal    |integer   |Season assist total for the primary assister.     |
#'    |assist2PlayerTotal    |integer   |Season assist total for the secondary assister.   |
#'    |penaltyTypeCode       |character |Penalty type code.                                |
#'    |descKey               |character |Penalty description key.                           |
#'    |duration              |integer   |Penalty duration in minutes.                      |
#'    |committedByPlayerId   |integer   |Player identifier who committed the penalty.      |
#'    |drawnByPlayerId       |integer   |Player identifier who drew the penalty.           |
#' @keywords NHL WSC PBP
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_wsc_pbp(game_id = 2023020001))
#' }
nhl_wsc_pbp <- function(game_id) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/wsc/play-by-play/{game_id}"
    )

    tryCatch(
        expr = {
            res <- .retry_request(url)
            check_status(res)

            resp_text <- .resp_text(res)
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            return(raw)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching WSC PBP for {game_id}: {e$message}"
            ))
            return(NULL)
        }
    )
}
