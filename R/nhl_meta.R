#' @title **NHL Meta**
#' @description Returns NHL metadata (players, teams, season states, game types).
#' @param game_id Optional game ID. If provided, returns metadata for that game.
#'   If NULL, returns general league meta.
#' @return Returns a list with NHL metadata.
#' @keywords NHL Meta
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_meta())
#' }
nhl_meta <- function(game_id = NULL) {
    if (is.null(game_id)) {
        url <- "https://api-web.nhle.com/v1/meta"
    } else {
        url <- glue::glue(
            "https://api-web.nhle.com/v1/meta/game/{game_id}"
        )
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
