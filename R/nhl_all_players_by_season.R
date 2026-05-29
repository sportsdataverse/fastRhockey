#' @title **NHL All Players by Season**
#' @description Aggregator helper that iterates every NHL team's
#'   season roster (via [nhl_teams_roster()]) and flattens forwards,
#'   defensemen, and goalies into a single tidy data frame. Mirrors the
#'   `Helpers.all_players` convenience helper from the `nhl-api-py`
#'   Python client.
#' @param season Integer four-digit year representing the start year
#'   of the season (e.g. `2024` for the 2024-25 season), matching the
#'   convention used by [nhl_teams_roster()].
#' @param sleep_rate Numeric seconds to `Sys.sleep()` between per-team
#'   API calls. Defaults to `0` (no delay).
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name        |types     |description                                  |
#'    |:---------------|:---------|:--------------------------------------------|
#'    |player_id       |integer   |Unique player identifier.                    |
#'    |first_name      |character |Player first name.                           |
#'    |last_name       |character |Player last name.                            |
#'    |full_name       |character |Player full name.                            |
#'    |sweater_number  |integer   |Jersey number.                               |
#'    |position_code   |character |Player position code.                        |
#'    |shoots_catches  |character |Handedness (shoots/catches).                 |
#'    |height_inches   |integer   |Player height in inches.                     |
#'    |weight_pounds   |integer   |Player weight in pounds.                     |
#'    |birth_date      |character |Player birth date.                           |
#'    |birth_city      |character |Player birth city.                           |
#'    |birth_country   |character |Player birth country.                        |
#'    |headshot_url    |character |URL to the player headshot image.            |
#'    |team_abbr       |character |Team abbreviation.                           |
#'    |season          |numeric   |Season start year (e.g., 2024 for 2024-25).  |
#'
#' @keywords NHL Helpers Aggregator
#' @importFrom dplyr bind_rows mutate
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_all_players_by_season(season = 2024))
#' }
nhl_all_players_by_season <- function(season, sleep_rate = 0) {
    tryCatch(
        expr = {
            teams_df <- nhl_teams()
            if (is.null(teams_df) || nrow(teams_df) == 0) {
                message(glue::glue(
                    "{Sys.time()}: Unable to fetch NHL teams list"
                ))
                return(NULL)
            }
            team_list <- unique(teams_df$team_abbr)

            rosters <- list()
            for (i in seq_along(team_list)) {
                tm <- team_list[i]
                r <- tryCatch(
                    expr = nhl_teams_roster(
                        team_abbr = tm,
                        season = season
                    ),
                    error = function(e) {
                        message(glue::glue(
                            "{Sys.time()}: Error fetching roster for {tm}: {e$message}"
                        ))
                        return(NULL)
                    }
                )

                if (!is.null(r) && nrow(r) > 0) {
                    r$team_abbr <- tm
                    r$season <- season
                    rosters[[length(rosters) + 1]] <- r
                }

                if (sleep_rate > 0 && i < length(team_list)) {
                    Sys.sleep(sleep_rate)
                }
            }

            if (length(rosters) == 0) {
                message(glue::glue(
                    "{Sys.time()}: No roster data aggregated for season {season}"
                ))
                return(NULL)
            }

            df <- dplyr::bind_rows(rosters)
            df <- make_fastRhockey_data(
                df,
                "NHL All Players by Season",
                Sys.time()
            )
            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error aggregating NHL rosters: {e$message}"
            ))
            return(NULL)
        }
    )
}
