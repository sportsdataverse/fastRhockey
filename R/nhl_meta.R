#' @title **NHL Meta**
#' @description Returns NHL metadata (players, teams, season states, game types).
#'
#' Three branches are supported:
#' \itemize{
#'   \item Both `year` and `series_letter` supplied — returns playoff-series
#'     metadata via `meta/playoff-series/{year}/{seriesLetter}`.
#'   \item Only `game_id` supplied — returns per-game metadata via
#'     `meta/game/{game_id}`.
#'   \item No arguments — returns general league meta via `meta`.
#' }
#' @param game_id Optional game ID. If provided (and `year` / `series_letter`
#'   are not), returns metadata for that game.
#' @param year Optional integer playoff year (e.g., `2024`). Must be supplied
#'   together with `series_letter` to fetch playoff-series metadata.
#' @param series_letter Optional single-letter playoff series identifier
#'   (e.g., `"a"`). Must be supplied together with `year`.
#' @return Returns a list with NHL metadata.
#' @keywords NHL Meta
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_meta())
#'   try(nhl_meta(game_id = 2024020001))
#'   try(nhl_meta(year = 2024, series_letter = "a"))
#' }
nhl_meta <- function(game_id = NULL, year = NULL, series_letter = NULL) {
    if (!is.null(year) && !is.null(series_letter)) {
        url <- glue::glue(
            "https://api-web.nhle.com/v1/meta/playoff-series/{year}/{series_letter}"
        )
    } else if (!is.null(game_id)) {
        url <- glue::glue(
            "https://api-web.nhle.com/v1/meta/game/{game_id}"
        )
    } else {
        url <- "https://api-web.nhle.com/v1/meta"
    }

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
                "{Sys.time()}: Error fetching meta: {e$message}"
            ))
            return(NULL)
        }
    )
}


#' @title **NHL Location**
#' @description Returns country/location configuration data from the NHL API.
#' @return Returns a list with location data.
#' @keywords NHL Location
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \donttest{
#'   try(nhl_location())
#' }
nhl_location <- function() {
    url <- "https://api-web.nhle.com/v1/location"

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
                "{Sys.time()}: Error fetching location: {e$message}"
            ))
            return(NULL)
        }
    )
}
