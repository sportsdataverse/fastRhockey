#' @title **NHL Club Stats Season**
#' @description Returns season-by-season stats metadata for a club, including
#' which game types (regular season, playoffs) are available for each season.
#' @param team_abbr Three-letter team abbreviation (e.g., "TOR", "BOS")
#' @return Returns a data frame with season/game-type availability.
#' @keywords NHL Club Stats Season
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_club_stats_season(team_abbr = "TOR"))
#' }
nhl_club_stats_season <- function(team_abbr) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/club-stats-season/{team_abbr}"
    )

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            if (is.null(raw) || length(raw) == 0) {
                message(glue::glue(
                    "{Sys.time()}: No club stats season data for {team_abbr}"
                ))
                return(NULL)
            }

            # Flatten nested structure
            if (is.data.frame(raw)) {
                df <- raw
            } else if (!is.null(raw[["seasons"]])) {
                df <- jsonlite::fromJSON(
                    jsonlite::toJSON(raw[["seasons"]], auto_unbox = TRUE),
                    flatten = TRUE
                )
            } else {
                df <- dplyr::as_tibble(raw)
            }

            df$team_abbr <- team_abbr
            df <- df %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Club Stats Season", Sys.time())

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching club stats season for {team_abbr}: {e$message}"
            ))
            return(NULL)
        }
    )
}
