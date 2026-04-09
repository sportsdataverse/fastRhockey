#' @title **NHL PPT Replay**
#' @description Returns event-level play-by-play replay metadata (including
#' video clip references) for a specific event in a given NHL game from the
#' `ppt-replay/{gameId}/{eventNumber}` endpoint.
#' @param game_id Integer or character game ID (e.g., 2023020001).
#' @param event_number Integer event / play number within the game.
#' @return Returns a list with event replay metadata (clip IDs, video URLs,
#'   event context).
#' @keywords NHL PPT Replay
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_ppt_replay(game_id = 2023020001, event_number = 1))
#' }
nhl_ppt_replay <- function(game_id, event_number) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/ppt-replay/{game_id}/{event_number}"
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
                "{Sys.time()}: Error fetching PPT replay for game {game_id} event {event_number}: {e$message}"
            ))
            return(NULL)
        }
    )
}
