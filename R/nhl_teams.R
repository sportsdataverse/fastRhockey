#' @title **NHL Teams**
#' @description Returns current NHL team information.
#' Uses the NHL API standings endpoint to get up-to-date team info.
#' @param season Integer four-digit year (e.g., 2024). If NULL, returns current teams.
#' @return Returns a data frame with team information.
#' @keywords NHL Teams
#' @importFrom jsonlite read_json
#' @importFrom dplyr tibble bind_rows mutate
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_teams())
#' }
nhl_teams <- function(season = NULL) {
    # Use standings endpoint to extract team data (most reliable source)
    if (is.null(season)) {
        url <- "https://api-web.nhle.com/v1/standings/now"
    } else {
        # For a specific date within a season, use a mid-season date
        date_str <- paste0(season + 1, "-01-15")
        url <- glue::glue("https://api-web.nhle.com/v1/standings/{date_str}")
    }

    tryCatch(
        expr = {
            raw <- jsonlite::read_json(url, simplifyVector = TRUE)
            standings <- raw[["standings"]]

            if (is.null(standings) || nrow(standings) == 0) {
                message(glue::glue("{Sys.time()}: No team data found"))
                return(NULL)
            }

            teams <- dplyr::tibble(
                team_abbr = standings$teamAbbrev$default,
                team_name = standings$teamName$default,
                team_common_name = standings$teamCommonName$default,
                team_logo = standings$teamLogo,
                conference_abbr = standings$conferenceName,
                conference_name = standings$conferenceName,
                division_abbr = standings$divisionAbbrev,
                division_name = standings$divisionName,
                place_name = standings$placeName$default,
                games_played = standings$gamesPlayed,
                wins = standings$wins,
                losses = standings$losses,
                ot_losses = standings$otLosses,
                points = standings$points,
                point_pctg = standings$pointPctg,
                goals_for = standings$goalFor,
                goals_against = standings$goalAgainst,
                goal_differential = standings$goalDifferential,
                streak_code = standings$streakCode,
                streak_count = standings$streakCount
            )

            teams <- make_fastRhockey_data(teams, "NHL Teams", Sys.time())
            return(teams)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching teams: {e$message}"
            ))
            return(NULL)
        }
    )
}
