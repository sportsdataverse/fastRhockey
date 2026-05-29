#' @title **NHL Player Spotlight**
#' @description Returns the current NHL player spotlight — featured players
#' highlighted by the league.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name       |types     |description                                |
#'    |:--------------|:---------|:------------------------------------------|
#'    |player_id      |integer   |Unique player identifier.                  |
#'    |player_slug    |character |URL slug for the player.                   |
#'    |position       |character |Player position.                           |
#'    |sweater_number |integer   |Player sweater (jersey) number.            |
#'    |team_id        |integer   |Unique team identifier.                    |
#'    |headshot       |character |URL of the player headshot image.          |
#'    |team_tri_code  |character |Three-letter team code.                    |
#'    |team_logo      |character |URL of the team logo image.                |
#'    |sort_id        |integer   |Sort order identifier for the spotlight.   |
#'    |name_default   |character |Player name (default localization).        |
#'    |name_cs        |character |Player name (Czech localization).          |
#'    |name_fi        |character |Player name (Finnish localization).        |
#'    |name_sk        |character |Player name (Slovak localization).         |
#' @keywords NHL Player Spotlight
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   try(nhl_player_spotlight())
#' }
nhl_player_spotlight <- function() {
    url <- "https://api-web.nhle.com/v1/player-spotlight"

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            if (is.null(raw) || length(raw) == 0) {
                message(paste0(
                    Sys.time(),
                    ": No player spotlight data available"
                ))
                return(NULL)
            }

            # raw is typically a data frame (array of player objects)
            if (is.data.frame(raw)) {
                spotlight_df <- raw
            } else {
                spotlight_df <- jsonlite::fromJSON(
                    jsonlite::toJSON(raw, auto_unbox = TRUE),
                    flatten = TRUE
                )
            }

            spotlight_df <- spotlight_df %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Player Spotlight", Sys.time())

            return(spotlight_df)
        },
        error = function(e) {
            message(paste0(
                Sys.time(),
                ": Error fetching player spotlight: ",
                e$message
            ))
            return(NULL)
        }
    )
}
