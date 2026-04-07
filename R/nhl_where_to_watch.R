#' @title **NHL Where to Watch**
#' @description Returns streaming/broadcast availability information.
#' @return Returns a list with streaming availability data.
#' @keywords NHL Where to Watch
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \donttest{
#' try(nhl_where_to_watch())
#' }
nhl_where_to_watch <- function() {
    url <- "https://api-web.nhle.com/v1/where-to-watch"

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
                "{Sys.time()}: Error fetching where to watch: {e$message}"
            ))
            return(NULL)
        }
    )
}


#' @title **NHL Partner Game Odds**
#' @description Returns partner game odds data for a country code.
#' @param country_code Two-letter country code (e.g., "US", "CA"). Default "US".
#' @return Returns a list with game odds data.
#' @keywords NHL Partner Game Odds
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#' try(nhl_partner_game_odds())
#' }
nhl_partner_game_odds <- function(country_code = "US") {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/partner-game/{country_code}/now"
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
                "{Sys.time()}: Error fetching partner game odds: {e$message}"
            ))
            return(NULL)
        }
    )
}
