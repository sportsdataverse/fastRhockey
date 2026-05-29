#' @title **NHL Draft by Year**
#' @description Returns draft pick information for a given year.
#'
#' Uses the NHL API endpoint at
#' `api-web.nhle.com/v1/draft/picks/{year}/{round}`.
#'
#' When `round` is `NULL` (default) or the literal string `"all"`, the
#' function takes the fast path and hits
#' `draft/picks/{year}/all` in a single request instead of looping over
#' rounds 1-7.
#'
#' @param year Integer. Draft year (e.g. 2023).
#' @param round Integer, character, or NULL. Specific round (1-7), the
#'   literal string `"all"`, or `NULL` (treated the same as `"all"`) to
#'   fetch every round at once.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                         |types      |description                                  |
#'    |:--------------------------------|:----------|:--------------------------------------------|
#'    |round                            |integer    |Draft round.                                 |
#'    |pick_in_round                    |integer    |Pick number within the round.                |
#'    |overall_pick                     |integer    |Overall pick number in the draft.            |
#'    |team_id                          |integer    |Unique team identifier.                      |
#'    |team_abbrev                      |character  |Team abbreviation.                           |
#'    |team_name                        |data.frame |Team name (localized variants).              |
#'    |team_common_name                 |data.frame |Team common name (localized variants).       |
#'    |team_place_name_with_preposition |data.frame |Team place name with preposition (localized).|
#'    |display_abbrev                   |data.frame |Display abbreviation (localized variants).   |
#'    |team_logo_light                  |character  |URL to the team logo (light variant).        |
#'    |team_logo_dark                   |character  |URL to the team logo (dark variant).         |
#'    |team_pick_history                |character  |Team pick history string.                    |
#'    |first_name                       |data.frame |Player first name (localized variants).      |
#'    |last_name                        |data.frame |Player last name (localized variants).       |
#'    |position_code                    |character  |Player position code.                        |
#'    |country_code                     |character  |Player country code.                         |
#'    |height                           |integer    |Player height in inches.                     |
#'    |weight                           |integer    |Player weight in pounds.                     |
#'    |amateur_league                   |character  |Amateur league played in.                    |
#'    |amateur_club_name                |character  |Amateur club played for.                     |
#' @keywords NHL Draft Year
#' @importFrom jsonlite read_json
#' @importFrom janitor clean_names
#' @importFrom dplyr bind_rows
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'    try(nhl_draft_year(year = 2023, round = 1))
#'    try(nhl_draft_year(year = 2023, round = "all"))
#' }
nhl_draft_year <- function(year, round = NULL) {
  tryCatch(
    expr = {
      all_rounds <- is.null(round) ||
        (is.character(round) && tolower(round) == "all")

      if (all_rounds) {
        # Fast path: fetch every round in a single request
        url <- glue::glue(
          "https://api-web.nhle.com/v1/draft/picks/{year}/all"
        )
        raw <- jsonlite::read_json(url, simplifyVector = TRUE)
        picks <- raw[["picks"]]
      } else {
        # Fetch a single numeric round
        url <- glue::glue(
          "https://api-web.nhle.com/v1/draft/picks/{year}/{round}"
        )
        raw <- jsonlite::read_json(url, simplifyVector = TRUE)
        picks <- raw[["picks"]]
      }

      if (is.null(picks) || (is.data.frame(picks) && nrow(picks) == 0) || length(picks) == 0) {
        message(glue::glue("{Sys.time()}: No draft data for {year} available!"))
        return(NULL)
      }

      draft_df <- as.data.frame(picks)
      draft_df <- draft_df %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Draft Year data from NHL.com", Sys.time())
      return(draft_df)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no draft data for {year} available!"))
      return(NULL)
    }
  )
}
