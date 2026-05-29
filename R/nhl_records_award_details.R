#' @title **NHL Records - Award Details**
#' @description Returns detailed NHL award winners from the NHL Records API
#'   (`https://records.nhl.com/site/api/award-details`). The endpoint
#'   accepts a Cayenne filter on `seasonId` (other paths such as
#'   `franchiseId` and `award-details/{id}` return errors).
#' @param season_id Optional integer 8-digit season ID (e.g.,
#'   `20232024`). When supplied, becomes the cayenne filter
#'   `seasonId={season_id}`. Ignored when `cayenne_exp` is also supplied.
#' @param cayenne_exp Optional Cayenne filter expression string passed via
#'   the `cayenneExp` query parameter.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name              |types     |description                              |
#'    |:---------------------|:---------|:----------------------------------------|
#'    |id                    |integer   |Unique award detail identifier.          |
#'    |awarded_posthumously  |logical   |Whether the award was given posthumously.|
#'    |coach_id              |integer   |Coach identifier, if a coach award.      |
#'    |created_on            |character |Timestamp the record was created.        |
#'    |detail_summary        |logical   |Detail summary flag.                     |
#'    |full_name             |character |Full name of the award winner.           |
#'    |general_manager_id    |integer   |General manager identifier, if applicable.|
#'    |image_url             |character |URL to the award image.                  |
#'    |is_rookie             |logical   |Whether the winner was a rookie.         |
#'    |player_id             |integer   |Unique player identifier.                |
#'    |player_image_caption  |logical   |Player image caption flag.               |
#'    |player_image_url      |character |URL to the player image.                 |
#'    |season_id             |integer   |8-digit season identifier.               |
#'    |status                |character |Award status.                            |
#'    |summary               |character |Award summary text.                      |
#'    |team_id               |integer   |Unique team identifier.                  |
#'    |trophy_category_id    |integer   |Trophy category identifier.              |
#'    |trophy_id             |integer   |Trophy identifier.                       |
#'    |value                 |integer   |Award value or statistical measure.      |
#'    |vote_count            |integer   |Number of votes received.                |
#' @keywords NHL Records Award Details
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_award_details())
#'   try(nhl_records_award_details(season_id = 20232024))
#' }
nhl_records_award_details <- function(
    season_id = NULL,
    cayenne_exp = NULL
) {
    if (is.null(cayenne_exp) && !is.null(season_id)) {
        cayenne_exp <- glue::glue("seasonId={as.integer(season_id)}")
    }
    raw <- .nhl_records_api(
        resource = "award-details",
        cayenne_exp = cayenne_exp
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No award details data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Award Details", Sys.time())
}
