#' @title **NHL Gamecenter Landing**
#' @description Returns the gamecenter landing page data for a given game,
#' including rosters, linescore, game state, and summary information.
#' @param game_id Integer or character game ID (e.g., 2024020001)
#' @return A named list of data frames: `tvBroadcasts`.
#'
#'    **tvBroadcasts**
#'
#'    |col_name       |types     |description                          |
#'    |:--------------|:---------|:------------------------------------|
#'    |id             |integer   |Unique broadcast identifier.         |
#'    |market         |character |Broadcast market (national/local).   |
#'    |countryCode    |character |Country code of the broadcast.       |
#'    |network        |character |Broadcasting network name.           |
#'    |sequenceNumber |integer   |Broadcast sequence order number.     |
#' @keywords NHL Gamecenter Landing
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_gamecenter_landing(game_id = 2024020001))
#' }
nhl_gamecenter_landing <- function(game_id) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/gamecenter/{game_id}/landing"
    )

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            return(raw)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching gamecenter landing for {game_id}: {e$message}"
            ))
            return(NULL)
        }
    )
}


#' @title **NHL Gamecenter Right Rail**
#' @description Returns the gamecenter right rail data for a given game,
#' including 3 stars, team leaders, and season series.
#' @param game_id Integer or character game ID (e.g., 2024020001)
#' @return A named list of data frames: `seasonSeries`, `shotsByPeriod`, `teamGameStats`.
#'
#'    **seasonSeries**
#'
#'    |col_name                              |types     |description                                  |
#'    |:-------------------------------------|:---------|:--------------------------------------------|
#'    |id                                    |integer   |Unique game identifier.                      |
#'    |season                               |integer   |Season (concluding year, YYYY).              |
#'    |gameType                              |integer   |Game type the row belongs to.                |
#'    |gameDate                              |character |Game date.                                   |
#'    |startTimeUTC                          |character |Game start time in UTC.                      |
#'    |easternUTCOffset                      |character |Eastern time UTC offset.                     |
#'    |venueUTCOffset                        |character |Venue local time UTC offset.                 |
#'    |gameState                             |character |Current game state.                          |
#'    |gameScheduleState                     |character |Schedule state of the game.                  |
#'    |gameCenterLink                        |character |Link to the gamecenter page.                 |
#'    |awayTeam.id                           |integer   |Away team unique identifier.                 |
#'    |awayTeam.abbrev                       |character |Away team abbreviation.                      |
#'    |awayTeam.logo                         |character |Away team logo URL.                          |
#'    |awayTeam.score                        |integer   |Away team score.                             |
#'    |homeTeam.id                           |integer   |Home team unique identifier.                 |
#'    |homeTeam.abbrev                       |character |Home team abbreviation.                      |
#'    |homeTeam.logo                         |character |Home team logo URL.                          |
#'    |homeTeam.score                        |integer   |Home team score.                             |
#'    |periodDescriptor.number               |integer   |Period number.                               |
#'    |periodDescriptor.periodType           |character |Period type (REG/OT/SO).                     |
#'    |periodDescriptor.maxRegulationPeriods |integer   |Maximum number of regulation periods.        |
#'    |gameOutcome.lastPeriodType            |character |Period type in which the game ended.         |
#'
#'    **shotsByPeriod**
#'
#'    |col_name                              |types     |description                                  |
#'    |:-------------------------------------|:---------|:--------------------------------------------|
#'    |away                                  |integer   |Away team shots in the period.               |
#'    |home                                  |integer   |Home team shots in the period.               |
#'    |periodDescriptor.number               |integer   |Period number.                               |
#'    |periodDescriptor.periodType           |character |Period type (REG/OT/SO).                     |
#'    |periodDescriptor.maxRegulationPeriods |integer   |Maximum number of regulation periods.        |
#'
#'    **teamGameStats**
#'
#'    |col_name   |types     |description                          |
#'    |:----------|:---------|:------------------------------------|
#'    |category   |character |Statistic category name.             |
#'    |awayValue  |character |Away team value for the category.    |
#'    |homeValue  |character |Home team value for the category.    |
#' @keywords NHL Gamecenter Right Rail
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_gamecenter_right_rail(game_id = 2024020001))
#' }
nhl_gamecenter_right_rail <- function(game_id) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/gamecenter/{game_id}/right-rail"
    )

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            return(raw)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching gamecenter right rail for {game_id}: {e$message}"
            ))
            return(NULL)
        }
    )
}
