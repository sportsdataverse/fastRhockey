#' @title **NHL Team Scoreboard**
#' @description Returns current scoreboard information for a specific team,
#' including upcoming and recent games.
#' @param team_abbr Three-letter team abbreviation (e.g., "TOR", "BOS")
#' @return Returns a list with scoreboard data including game info.
#' @keywords NHL Team Scoreboard
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_team_scoreboard(team_abbr = "TOR"))
#' }
nhl_team_scoreboard <- function(team_abbr) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/scoreboard/{team_abbr}/now"
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
                "{Sys.time()}: Error fetching team scoreboard for {team_abbr}: {e$message}"
            ))
            return(NULL)
        }
    )
}
