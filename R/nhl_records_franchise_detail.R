#' @title **NHL Records - Franchise Detail**
#' @description Returns detailed franchise metadata from the NHL Records API
#'   (`https://records.nhl.com/site/api/franchise-detail`). Optionally filter
#'   by franchise ID or a `cayenneExp` expression.
#' @param franchise_id Optional integer franchise ID. When supplied it is
#'   translated into a `cayenneExp=mostRecentTeamId={franchise_id}` filter
#'   unless `cayenne_exp` is already provided.
#' @param cayenne_exp Optional Cayenne filter expression string. Takes
#'   precedence over `franchise_id` when both are supplied.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                 |types     |description                            |
#'    |:------------------------|:---------|:--------------------------------------|
#'    |id                       |integer   |Unique franchise detail identifier.    |
#'    |active                   |logical   |Whether the franchise is active.       |
#'    |captain_history          |character |Franchise captain history text.        |
#'    |coaching_history         |character |Franchise coaching history text.       |
#'    |date_awarded             |character |Date the franchise was awarded.        |
#'    |directory_url            |character |Franchise directory URL.               |
#'    |first_season_id          |integer   |Season identifier of the first season. |
#'    |general_manager_history  |character |Franchise general manager history text.|
#'    |hero_image_url           |character |Franchise hero image URL.              |
#'    |most_recent_team_id      |integer   |Most recent team identifier.           |
#'    |retired_numbers_summary  |character |Summary of retired jersey numbers.     |
#'    |team_abbrev              |character |Team abbreviation.                     |
#'    |team_full_name           |character |Full team name.                        |
#' @keywords NHL Records Franchise Detail
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_franchise_detail())
#' }
nhl_records_franchise_detail <- function(
    franchise_id = NULL,
    cayenne_exp = NULL
) {
    if (is.null(cayenne_exp) && !is.null(franchise_id)) {
        cayenne_exp <- glue::glue("mostRecentTeamId={franchise_id}")
    }
    raw <- .nhl_records_api(
        resource = "franchise-detail",
        cayenne_exp = cayenne_exp
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No franchise detail data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Franchise Detail", Sys.time())
}
