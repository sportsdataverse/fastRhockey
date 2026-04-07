#' @title **NHL Team Roster**
#' @description Returns the roster for a given NHL team.
#' Uses the NHL API (`api-web.nhle.com`).
#' @param team_abbr Character three-letter team abbreviation (e.g., "TOR").
#' @param season Integer four-digit year (e.g., 2024 for the 2024-25 season).
#'   If NULL, returns the current roster.
#' @return Returns a data frame with roster information.
#' @keywords NHL Roster
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr tibble bind_rows mutate
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_teams_roster(team_abbr = "TOR"))
#'   try(nhl_teams_roster(team_abbr = "TOR", season = 2024))
#' }
nhl_teams_roster <- function(team_abbr, season = NULL) {
    if (is.null(season)) {
        url <- glue::glue(
            "https://api-web.nhle.com/v1/roster/{team_abbr}/current"
        )
    } else {
        season_str <- paste0(season, season + 1)
        url <- glue::glue(
            "https://api-web.nhle.com/v1/roster/{team_abbr}/{season_str}"
        )
    }

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            parse_position_group <- function(players, position) {
                if (is.null(players) || length(players) == 0) {
                    return(NULL)
                }
                dplyr::tibble(
                    player_id = players$id,
                    first_name = players$firstName.default,
                    last_name = players$lastName.default,
                    full_name = paste(
                        players$firstName.default,
                        players$lastName.default
                    ),
                    sweater_number = as.integer(players$sweaterNumber),
                    position_code = position,
                    shoots_catches = players$shootsCatches,
                    height_inches = players$heightInInches,
                    weight_pounds = players$weightInPounds,
                    birth_date = players$birthDate,
                    birth_city = players$birthCity.default,
                    birth_country = players$birthCountry,
                    headshot_url = players$headshot
                )
            }

            roster <- dplyr::bind_rows(
                parse_position_group(raw$forwards, "F"),
                parse_position_group(raw$defensemen, "D"),
                parse_position_group(raw$goalies, "G")
            )

            if (is.null(roster) || nrow(roster) == 0) {
                message(glue::glue(
                    "{Sys.time()}: No roster found for {team_abbr}"
                ))
                return(NULL)
            }

            roster$team_abbr <- team_abbr
            roster <- make_fastRhockey_data(roster, "NHL Roster", Sys.time())
            return(roster)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching roster for {team_abbr}: {e$message}"
            ))
            return(NULL)
        }
    )
}
