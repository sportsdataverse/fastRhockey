#' @title **NHL Game Story**
#' @description Returns the game story / recap for a given game ID from the
#' NHL web service.
#' @param game_id Integer or character game ID (e.g., 2024020001)
#' @return A named list with game story data (recap, scoring, penalties, etc.).
#' The `tvBroadcasts` element is a data frame with the following columns:
#'
#'    **tvBroadcasts**
#'
#'    |col_name       |types     |description                          |
#'    |:--------------|:---------|:------------------------------------|
#'    |id             |integer   |Broadcast identifier.                |
#'    |market         |character |Broadcast market (e.g. home/away).   |
#'    |countryCode    |character |Country code of the broadcast.       |
#'    |network        |character |Broadcasting network name.           |
#'    |sequenceNumber |integer   |Order of the broadcast listing.      |
#' @keywords NHL Game Story
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_game_story(game_id = 2024020001))
#' }
nhl_game_story <- function(game_id) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/wsc/game-story/{game_id}"
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
                "{Sys.time()}: Error fetching game story for {game_id}: {e$message}"
            ))
            return(NULL)
        }
    )
}
