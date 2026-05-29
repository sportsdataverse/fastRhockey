#' @title **NHL Records - Draft Listing**
#' @description Returns the NHL Entry Draft listing from the NHL Records API
#'   (`https://records.nhl.com/site/api/draft`). Supports Cayenne filtering
#'   and pagination.
#' @param cayenne_exp Optional Cayenne filter expression string (e.g.
#'   `"draftYear=2015"`).
#' @param limit Optional integer page size.
#' @param start Optional integer pagination offset.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name             |types     |description                              |
#'    |:--------------------|:---------|:----------------------------------------|
#'    |id                   |integer   |Unique draft record identifier.          |
#'    |age_in_days          |integer   |Player age in days.                      |
#'    |age_in_days_for_year |integer   |Player age in days for the draft year.   |
#'    |age_in_years         |integer   |Player age in years.                     |
#'    |amateur_club_name    |character |Amateur club the player played for.      |
#'    |amateur_league       |character |Amateur league the player played in.     |
#'    |birth_date           |character |Player birth date.                       |
#'    |birth_place          |character |Player birth place.                      |
#'    |country_code         |character |Player country code.                     |
#'    |cs_player_id         |integer   |Central Scouting player identifier.      |
#'    |draft_date           |character |Date the player was drafted.             |
#'    |draft_master_id      |integer   |Draft master record identifier.          |
#'    |draft_year           |integer   |Year of the draft.                       |
#'    |drafted_by_team_id   |integer   |Identifier of the drafting team.         |
#'    |first_name           |character |Player first name.                       |
#'    |height               |integer   |Player height in inches.                 |
#'    |last_name            |character |Player last name.                        |
#'    |notes                |logical   |Notes flag for the pick.                 |
#'    |overall_pick_number  |integer   |Overall pick number in the draft.        |
#'    |pick_in_round        |integer   |Pick number within the round.            |
#'    |player_id            |integer   |Unique player identifier.                |
#'    |player_name          |character |Player full name.                        |
#'    |position             |character |Player position.                         |
#'    |removed_outright     |character |Removed-outright indicator.              |
#'    |removed_outright_why |logical   |Reason the pick was removed outright.    |
#'    |round_number         |integer   |Draft round number.                      |
#'    |shoots_catches       |character |Player shooting/catching hand.           |
#'    |supplemental_draft   |character |Supplemental draft indicator.            |
#'    |team_pick_history    |character |History of the team's pick.              |
#'    |tri_code             |character |Three-letter team code.                  |
#'    |weight               |integer   |Player weight in pounds.                 |
#' @keywords NHL Records Draft
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_draft(cayenne_exp = "draftYear=2020", limit = 5))
#' }
nhl_records_draft <- function(
    cayenne_exp = NULL,
    limit = NULL,
    start = NULL
) {
    raw <- .nhl_records_api(
        resource = "draft",
        cayenne_exp = cayenne_exp,
        limit = limit,
        start = start
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No draft data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Draft", Sys.time())
}
