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
#' @return A `fastRhockey_data` tibble of award details, or `NULL` on failure.
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
