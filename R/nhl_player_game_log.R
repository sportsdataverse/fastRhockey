#' @title **NHL Player Game Log**
#' @description Returns game-by-game stats for an NHL player. Supports both
#' current season and historical season lookups.
#' @param player_id Integer player ID (e.g., 8478402 for Connor McDavid)
#' @param season Integer 4-digit year (e.g., 2024 for the 2024-25 season).
#'   If NULL, returns the current season game log.
#' @param game_type Integer game type: 2 = regular season (default), 3 = playoffs
#' @return Returns a data frame with game-by-game statistics.
#' @keywords NHL Player Game Log
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_player_game_log(player_id = 8478402))
#' }
nhl_player_game_log <- function(player_id, season = NULL, game_type = 2) {
    if (is.null(season)) {
        url <- glue::glue(
            "https://api-web.nhle.com/v1/player/{player_id}/game-log/now"
        )
    } else {
        api_season <- paste0(season, season + 1)
        url <- glue::glue(
            "https://api-web.nhle.com/v1/player/{player_id}/game-log/{api_season}/{game_type}"
        )
    }

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            game_log <- raw$gameLog
            if (
                is.null(game_log) ||
                    length(game_log) == 0 ||
                    nrow(game_log) == 0
            ) {
                message(glue::glue(
                    "{Sys.time()}: No game log data for player {player_id}"
                ))
                return(NULL)
            }

            game_log$player_id <- player_id
            game_log <- game_log %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Player Game Log", Sys.time())

            return(game_log)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching game log for player {player_id}: {e$message}"
            ))
            return(NULL)
        }
    )
}
