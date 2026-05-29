#' @title **NHL Records - Draft Prospect Listing**
#' @description Returns the draft prospect listing from the NHL Records API
#'   (`https://records.nhl.com/site/api/draft-prospect`). Optionally filter
#'   to a single prospect via `prospect_id` (switches resource to
#'   `draft-prospect/{prospect_id}`).
#' @param prospect_id Optional integer prospect ID. If supplied, the resource
#'   becomes `draft-prospect/{prospect_id}`.
#' @param cayenne_exp Optional Cayenne filter expression string. Ignored when
#'   `prospect_id` is supplied.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name              |types     |description                                   |
#'    |:---------------------|:---------|:---------------------------------------------|
#'    |id                    |integer   |Unique prospect record identifier.            |
#'    |birth_city            |character |Prospect birth city.                          |
#'    |birth_country3code    |character |Prospect birth country three-letter code.     |
#'    |birth_date            |character |Prospect date of birth.                       |
#'    |birth_state_prov_code |character |Prospect birth state/province code.           |
#'    |category_id           |integer   |Prospect category identifier.                 |
#'    |created_on            |character |Record creation timestamp.                    |
#'    |cs_player_id          |integer   |Central Scouting player identifier.           |
#'    |draft_status_code     |character |Draft eligibility status code.                |
#'    |ep_player_id          |integer   |EliteProspects player identifier.             |
#'    |first_name            |character |Prospect first name.                          |
#'    |headshot_id           |integer   |Headshot image identifier.                    |
#'    |height                |integer   |Prospect height (inches).                     |
#'    |hometown              |character |Prospect hometown.                            |
#'    |last_club_name        |character |Most recent club name.                        |
#'    |last_league_abbr      |character |Most recent league abbreviation.              |
#'    |last_name             |character |Prospect last name.                           |
#'    |nationality_code      |character |Prospect nationality code.                    |
#'    |news_articles         |character |Associated news articles.                     |
#'    |playerid              |integer   |Unique player identifier.                     |
#'    |position_desc         |character |Player position description.                  |
#'    |profile               |character |Prospect profile text.                        |
#'    |quotes                |character |Quotes about the prospect.                    |
#'    |scouting_report       |character |Scouting report text.                         |
#'    |shoots_catches        |character |Shooting/catching handedness.                 |
#'    |stats_text            |character |Statistical summary text.                     |
#'    |video                 |character |Associated video content.                     |
#'    |weight                |integer   |Prospect weight (pounds).                     |
#' @keywords NHL Records Draft Prospect
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_draft_prospect())
#' }
nhl_records_draft_prospect <- function(
    prospect_id = NULL,
    cayenne_exp = NULL
) {
    if (!is.null(prospect_id)) {
        raw <- .nhl_records_api(
            resource = glue::glue("draft-prospect/{prospect_id}")
        )
    } else {
        raw <- .nhl_records_api(
            resource = "draft-prospect",
            cayenne_exp = cayenne_exp
        )
    }
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No draft prospect data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Draft Prospect", Sys.time())
}
