#' @title **NHL WSC Play-by-Play**
#' @description Returns the narrative-format (Web Services Content) play-by-play
#' feed for a given NHL game ID from the NHL web service.
#'
#' This endpoint is distinct from [`nhl_game_pbp()`], which hits the
#' `gamecenter/{id}/play-by-play` endpoint and returns a tabular play-event
#' feed. `nhl_wsc_pbp()` hits `wsc/play-by-play/{gameId}` and returns the
#' narrative-format payload used by NHL.com's story / recap pages (closer in
#' shape to [`nhl_game_story()`]).
#' @param game_id Integer or character game ID (e.g., 2023020001).
#' @return Returns a list with narrative-format play-by-play data.
#' @keywords NHL WSC PBP
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_wsc_pbp(game_id = 2023020001))
#' }
nhl_wsc_pbp <- function(game_id) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/wsc/play-by-play/{game_id}"
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
                "{Sys.time()}: Error fetching WSC PBP for {game_id}: {e$message}"
            ))
            return(NULL)
        }
    )
}
