#' @title **NHL Playoff Carousel**
#' @description Returns playoff series carousel data for a given season.
#' @param season Integer 4-digit year (e.g., 2024 for the 2024-25 season).
#'   If NULL, uses most recent season.
#' @return A named list of data frames: `rounds`.
#'
#'    **rounds**
#'
#'    |col_name    |types     |description                                |
#'    |:-----------|:---------|:------------------------------------------|
#'    |roundNumber |integer   |Playoff round number.                      |
#'    |roundLabel  |character |Full label for the playoff round.          |
#'    |roundAbbrev |character |Abbreviated label for the playoff round.   |
#'    |series      |list      |Nested list of series within the round.    |
#' @keywords NHL Playoff Carousel
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_playoff_carousel(season = 2024))
#' }
nhl_playoff_carousel <- function(season = NULL) {
    if (is.null(season)) {
        season <- most_recent_nhl_season()
    }
    if (nchar(as.character(season)) >= 8) {
        api_season <- as.character(season)
    } else {
        api_season <- paste0(season, as.integer(season) + 1)
    }
    url <- glue::glue(
        "https://api-web.nhle.com/v1/playoff-series/carousel/{api_season}/"
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
                "{Sys.time()}: Error fetching playoff carousel: {e$message}"
            ))
            return(NULL)
        }
    )
}


#' @title **NHL Playoff Schedule**
#' @description Returns the playoff schedule for a specific round/series.
#' @param season Integer 4-digit year (e.g., 2024 for the 2024-25 season).
#' @param series_letter Character series letter (e.g., "a", "b", "c", "d").
#' @return A named list of data frames: `games`.
#'
#'    **games**
#'
#'    |col_name                                  |types     |description                                      |
#'    |:-----------------------------------------|:---------|:------------------------------------------------|
#'    |id                                        |integer   |Unique game identifier.                          |
#'    |season                                    |integer   |Season (concluding year, YYYY).                  |
#'    |gameType                                  |integer   |Game type identifier (3 for playoffs).           |
#'    |gameNumber                                |integer   |Game number within the series.                   |
#'    |ifNecessary                               |logical   |Whether the game is played only if necessary.    |
#'    |neutralSite                               |logical   |Whether the game is at a neutral site.           |
#'    |startTimeUTC                              |character |Scheduled start time in UTC.                     |
#'    |easternUTCOffset                          |character |UTC offset for US Eastern time.                  |
#'    |venueUTCOffset                            |character |UTC offset at the venue.                         |
#'    |venueTimezone                             |character |Time zone of the venue.                          |
#'    |gameState                                 |character |Current game state.                              |
#'    |gameScheduleState                         |character |Schedule state of the game.                      |
#'    |tvBroadcasts                              |list      |Nested list of TV broadcast details.             |
#'    |gameCenterLink                            |character |Relative link to the game center page.           |
#'    |venue.default                             |character |Venue name (default localization).               |
#'    |awayTeam.id                               |integer   |Away team unique identifier.                     |
#'    |awayTeam.abbrev                           |character |Away team abbreviation.                          |
#'    |awayTeam.score                            |integer   |Away team score.                                 |
#'    |awayTeam.commonName.default               |character |Away team common name (default localization).    |
#'    |awayTeam.commonName.fr                    |character |Away team common name (French localization).     |
#'    |awayTeam.placeName.default                |character |Away team place name (default localization).     |
#'    |awayTeam.placeNameWithPreposition.default |character |Away team place name with preposition (default). |
#'    |awayTeam.placeNameWithPreposition.fr      |character |Away team place name with preposition (French).  |
#'    |homeTeam.id                               |integer   |Home team unique identifier.                     |
#'    |homeTeam.abbrev                           |character |Home team abbreviation.                          |
#'    |homeTeam.score                            |integer   |Home team score.                                 |
#'    |homeTeam.commonName.default               |character |Home team common name (default localization).    |
#'    |homeTeam.commonName.fr                    |character |Home team common name (French localization).     |
#'    |homeTeam.placeName.default                |character |Home team place name (default localization).     |
#'    |homeTeam.placeNameWithPreposition.default |character |Home team place name with preposition (default). |
#'    |homeTeam.placeNameWithPreposition.fr      |character |Home team place name with preposition (French).  |
#'    |periodDescriptor.number                   |integer   |Period number for the game state.                |
#'    |periodDescriptor.periodType               |character |Period type (e.g., REG, OT).                     |
#'    |periodDescriptor.maxRegulationPeriods     |integer   |Maximum number of regulation periods.            |
#'    |seriesStatus.topSeedWins                  |integer   |Series wins by the top-seeded team.              |
#'    |seriesStatus.bottomSeedWins               |integer   |Series wins by the bottom-seeded team.           |
#'    |gameOutcome.lastPeriodType                |character |Period type in which the game ended.             |
#'    |gameOutcome.otPeriods                     |integer   |Number of overtime periods played.               |
#' @keywords NHL Playoff Schedule
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_playoff_schedule(season = 2024, series_letter = "a"))
#' }
nhl_playoff_schedule <- function(season, series_letter) {
    if (nchar(as.character(season)) >= 8) {
        api_season <- as.character(season)
    } else {
        api_season <- paste0(season, as.integer(season) + 1)
    }
    .fetch_playoff_series(api_season, series_letter, flatten = TRUE)
}


#' @title **NHL Playoff Bracket**
#' @description Returns the complete playoff bracket for a given year.
#' @param year Integer 4-digit year (e.g., 2024 for the 2024 playoffs).
#' @return A named list of data frames: `series`.
#'
#'    **series**
#'
#'    |col_name                                         |types     |description                                       |
#'    |:------------------------------------------------|:---------|:-------------------------------------------------|
#'    |seriesUrl                                        |character |Relative URL of the series page.                  |
#'    |seriesTitle                                      |character |Full title of the series.                         |
#'    |seriesAbbrev                                     |character |Abbreviated series title.                         |
#'    |seriesLetter                                     |character |Series letter identifier.                         |
#'    |playoffRound                                     |integer   |Playoff round number.                             |
#'    |topSeedRank                                      |integer   |Seed rank of the top-seeded team.                 |
#'    |topSeedRankAbbrev                                |character |Abbreviated seed rank of the top-seeded team.     |
#'    |topSeedWins                                      |integer   |Series wins by the top-seeded team.               |
#'    |bottomSeedRank                                   |integer   |Seed rank of the bottom-seeded team.              |
#'    |bottomSeedRankAbbrev                             |character |Abbreviated seed rank of the bottom-seeded team.  |
#'    |bottomSeedWins                                   |integer   |Series wins by the bottom-seeded team.            |
#'    |winningTeamId                                    |integer   |Unique identifier of the series-winning team.     |
#'    |losingTeamId                                     |integer   |Unique identifier of the series-losing team.      |
#'    |seriesLogo                                       |character |URL of the series logo image.                     |
#'    |seriesLogoFr                                     |character |URL of the series logo image (French).            |
#'    |conferenceAbbrev                                 |character |Conference abbreviation.                          |
#'    |conferenceName                                   |character |Conference name.                                  |
#'    |topSeedTeam.id                                   |integer   |Top-seeded team unique identifier.                |
#'    |topSeedTeam.abbrev                               |character |Top-seeded team abbreviation.                     |
#'    |topSeedTeam.logo                                 |character |Top-seeded team logo URL.                         |
#'    |topSeedTeam.darkLogo                             |character |Top-seeded team dark logo URL.                    |
#'    |topSeedTeam.name.default                         |character |Top-seeded team name (default localization).      |
#'    |topSeedTeam.name.fr                              |character |Top-seeded team name (French localization).       |
#'    |topSeedTeam.commonName.default                   |character |Top-seeded team common name (default).            |
#'    |topSeedTeam.placeNameWithPreposition.default     |character |Top-seeded team place name with preposition.      |
#'    |topSeedTeam.placeNameWithPreposition.fr          |character |Top-seeded team place name with preposition (FR). |
#'    |bottomSeedTeam.id                                |integer   |Bottom-seeded team unique identifier.             |
#'    |bottomSeedTeam.abbrev                            |character |Bottom-seeded team abbreviation.                  |
#'    |bottomSeedTeam.logo                              |character |Bottom-seeded team logo URL.                      |
#'    |bottomSeedTeam.darkLogo                          |character |Bottom-seeded team dark logo URL.                 |
#'    |bottomSeedTeam.name.default                      |character |Bottom-seeded team name (default localization).   |
#'    |bottomSeedTeam.name.fr                           |character |Bottom-seeded team name (French localization).    |
#'    |bottomSeedTeam.commonName.default                |character |Bottom-seeded team common name (default).         |
#'    |bottomSeedTeam.placeNameWithPreposition.default  |character |Bottom-seeded team place name with preposition.   |
#'    |bottomSeedTeam.placeNameWithPreposition.fr       |character |Bottom-seeded team place name with preposition (FR).|
#' @keywords NHL Playoff Bracket
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_playoff_bracket(year = 2024))
#' }
nhl_playoff_bracket <- function(year) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/playoff-bracket/{year}"
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
                "{Sys.time()}: Error fetching playoff bracket for {year}: {e$message}"
            ))
            return(NULL)
        }
    )
}
