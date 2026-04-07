#' @title **NHL Playoff Carousel**
#' @description Returns playoff series carousel data for a given season.
#' @param season Integer 4-digit year (e.g., 2024 for the 2024-25 season).
#'   If NULL, uses most recent season.
#' @return Returns a list with playoff series carousel data.
#' @keywords NHL Playoff Carousel
#' @importFrom httr RETRY content
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
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
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
#' @return Returns a list with playoff schedule data for the series.
#' @keywords NHL Playoff Schedule
#' @importFrom httr RETRY content
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
    url <- glue::glue(
        "https://api-web.nhle.com/v1/schedule/playoff-series/{api_season}/{series_letter}/"
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
                "{Sys.time()}: Error fetching playoff schedule: {e$message}"
            ))
            return(NULL)
        }
    )
}


#' @title **NHL Playoff Bracket**
#' @description Returns the complete playoff bracket for a given year.
#' @param year Integer 4-digit year (e.g., 2024 for the 2024 playoffs).
#' @return Returns a list with complete bracket data (series, matchups, results).
#' @keywords NHL Playoff Bracket
#' @importFrom httr RETRY content
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
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
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
