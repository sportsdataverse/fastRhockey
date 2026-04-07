#' @title **NHL Game Story**
#' @description Returns the game story / recap for a given game ID from the
#' NHL web service.
#' @param game_id Integer or character game ID (e.g., 2024020001)
#' @return Returns a list with game story data (recap, scoring, penalties, etc.)
#' @keywords NHL Game Story
#' @importFrom httr RETRY content
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
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
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
