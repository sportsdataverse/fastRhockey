#' @title **NHL Team Roster**
#' @description Returns the roster for a given NHL team.
#' Uses the NHL API (`api-web.nhle.com`).
#' @param team_abbr Character three-letter team abbreviation (e.g., "TOR").
#' @param season Integer four-digit year (e.g., 2024 for the 2024-25 season).
#'   If NULL, returns the current roster.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name       |types     |description                                  |
#'    |:--------------|:---------|:--------------------------------------------|
#'    |player_id      |integer   |Unique player identifier.                    |
#'    |first_name     |character |Player first name.                           |
#'    |last_name      |character |Player last name.                            |
#'    |full_name      |character |Player full name.                            |
#'    |sweater_number |integer   |Jersey number.                               |
#'    |position_code  |character |Player position code (F/D/G).                |
#'    |shoots_catches |character |Handedness (shoots/catches).                 |
#'    |height_inches  |integer   |Height in inches.                            |
#'    |weight_pounds  |integer   |Weight in pounds.                            |
#'    |birth_date     |character |Player birth date.                           |
#'    |birth_city     |character |Player birth city.                           |
#'    |birth_country  |character |Player birth country.                        |
#'    |headshot_url   |character |URL to the player's headshot image.          |
#'    |team_abbr      |character |Team abbreviation.                           |
#' @keywords NHL Roster
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
            res <- .retry_request(url)
            check_status(res)

            resp_text <- .resp_text(res)
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
