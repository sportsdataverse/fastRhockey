#' @title **NHL PPT Replay (Goal)**
#' @description Returns goal-specific play-by-play replay metadata (including
#' video clip references) for a goal event in a given NHL game from the
#' `ppt-replay/goal/{gameId}/{eventNumber}` endpoint.
#' @param game_id Integer or character game ID (e.g., 2023020001).
#' @param event_number Integer event / play number within the game that
#'   corresponds to a goal.
#' @return Returns a list with goal replay metadata (clip IDs, video URLs,
#'   scoring context).
#' @keywords NHL PPT Replay Goal
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_ppt_replay_goal(game_id = 2023020001, event_number = 1))
#' }
nhl_ppt_replay_goal <- function(game_id, event_number) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/ppt-replay/goal/{game_id}/{event_number}"
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
                "{Sys.time()}: Error fetching PPT goal replay for game {game_id} event {event_number}: {e$message}"
            ))
            return(NULL)
        }
    )
}
