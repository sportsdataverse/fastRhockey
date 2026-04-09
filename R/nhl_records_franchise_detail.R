#' @title **NHL Records - Franchise Detail**
#' @description Returns detailed franchise metadata from the NHL Records API
#'   (`https://records.nhl.com/site/api/franchise-detail`). Optionally filter
#'   by franchise ID or a `cayenneExp` expression.
#' @param franchise_id Optional integer franchise ID. When supplied it is
#'   translated into a `cayenneExp=mostRecentTeamId={franchise_id}` filter
#'   unless `cayenne_exp` is already provided.
#' @param cayenne_exp Optional Cayenne filter expression string. Takes
#'   precedence over `franchise_id` when both are supplied.
#' @return A `fastRhockey_data` tibble of franchise details, or `NULL` on failure.
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
