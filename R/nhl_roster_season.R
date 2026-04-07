#' @title **NHL Roster Season**
#' @description Returns the list of seasons for which roster data is available
#' for a given team.
#' @param team_abbr Three-letter team abbreviation (e.g., "TOR", "BOS")
#' @return Returns a data frame with available roster seasons.
#' @keywords NHL Roster Season
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom dplyr tibble
#' @export
#' @examples
#' \donttest{
#'   try(nhl_roster_season(team_abbr = "TOR"))
#' }
nhl_roster_season <- function(team_abbr) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/roster-season/{team_abbr}"
    )

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            if (is.null(raw) || length(raw) == 0) {
                message(glue::glue(
                    "{Sys.time()}: No roster season data for {team_abbr}"
                ))
                return(NULL)
            }

            # raw is typically a vector of season integers (e.g., 20102011, 20112012, ...)
            if (is.atomic(raw)) {
                df <- dplyr::tibble(season = raw, team_abbr = team_abbr)
            } else {
                df <- dplyr::as_tibble(raw)
                df$team_abbr <- team_abbr
            }

            df <- df %>%
                make_fastRhockey_data("NHL Roster Season", Sys.time())

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching roster season for {team_abbr}: {e$message}"
            ))
            return(NULL)
        }
    )
}
