#' @title **NHL Team Scoreboard**
#' @description Returns current scoreboard information for a specific team,
#' including upcoming and recent games.
#' @param team_abbr Three-letter team abbreviation (e.g., "TOR", "BOS")
#' @return A named list of data frames: `gamesByDate`.
#'
#'    **gamesByDate**
#'
#'    |col_name |types     |description                                  |
#'    |:--------|:---------|:--------------------------------------------|
#'    |date     |character |Date the games are scheduled for.            |
#'    |games    |list      |List of games scheduled on that date.        |
#' @keywords NHL Team Scoreboard
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
            res <- .retry_request(url)
            check_status(res)

            resp_text <- .resp_text(res)
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
