#' @title **NHL Gamecenter Landing**
#' @description Returns the gamecenter landing page data for a given game,
#' including rosters, linescore, game state, and summary information.
#' @param game_id Integer or character game ID (e.g., 2024020001)
#' @return Returns a list with gamecenter landing data.
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
#' @return Returns a list with gamecenter right rail data.
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
