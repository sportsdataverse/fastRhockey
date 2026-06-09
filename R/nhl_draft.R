#' @title **NHL Draft**
#' @description Returns information on the most recent NHL draft picks.
#'
#' Uses the new NHL API endpoint at `api-web.nhle.com/v1/draft/picks/now`.
#'
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                         |types      |description                                       |
#'    |:--------------------------------|:----------|:-------------------------------------------------|
#'    |round                            |integer    |Draft round number.                               |
#'    |pick_in_round                    |integer    |Pick number within the round.                     |
#'    |overall_pick                     |integer    |Overall pick number in the draft.                 |
#'    |team_id                          |integer    |Unique team identifier of the drafting team.      |
#'    |team_abbrev                      |character  |Drafting team abbreviation.                       |
#'    |team_name                        |data.frame |Drafting team name (localized).                   |
#'    |team_common_name                 |data.frame |Drafting team common (nickname) name (localized). |
#'    |team_place_name_with_preposition |data.frame |Drafting team place name with preposition.        |
#'    |display_abbrev                   |data.frame |Drafting team display abbreviation.               |
#'    |team_logo_light                  |character  |URL to the team's light-theme logo.               |
#'    |team_logo_dark                   |character  |URL to the team's dark-theme logo.                |
#'    |team_pick_history                |character  |History of the team's picks at this slot.         |
#'    |first_name                       |data.frame |Drafted player's first name (localized).          |
#'    |last_name                        |data.frame |Drafted player's last name (localized).           |
#'    |position_code                    |character  |Drafted player's position code.                   |
#'    |country_code                     |character  |Drafted player's country code.                    |
#'    |height                           |integer    |Drafted player's height in inches.                |
#'    |weight                           |integer    |Drafted player's weight in pounds.                |
#'    |amateur_league                   |character  |Drafted player's amateur league.                  |
#'    |amateur_club_name                |character  |Drafted player's amateur club name.               |
#' @keywords NHL Draft
#' @importFrom jsonlite read_json
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'    try(nhl_draft())
#' }
nhl_draft <- function() {
  url <- "https://api-web.nhle.com/v1/draft/picks/now"

  tryCatch(
    expr = {
      raw <- jsonlite::read_json(url, simplifyVector = TRUE)
      picks <- raw[["picks"]]
      if (is.null(picks) || length(picks) == 0) {
        # try top-level if "picks" key doesn't exist
        if (is.data.frame(raw)) {
          picks <- raw
        } else {
          message(glue::glue("{Sys.time()}: No draft picks data available!"))
          return(NULL)
        }
      }
      draft_df <- as.data.frame(picks)
      draft_df <- draft_df %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Draft Data from NHL.com", Sys.time())
      return(draft_df)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no draft data available!"))
      return(NULL)
    }
  )
}
