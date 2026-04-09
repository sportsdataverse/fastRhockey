#' @title **NHL Records - Draft Listing**
#' @description Returns the NHL Entry Draft listing from the NHL Records API
#'   (`https://records.nhl.com/site/api/draft`). Supports Cayenne filtering
#'   and pagination.
#' @param cayenne_exp Optional Cayenne filter expression string (e.g.
#'   `"draftYear=2015"`).
#' @param limit Optional integer page size.
#' @param start Optional integer pagination offset.
#' @return A `fastRhockey_data` tibble of draft picks, or `NULL` on failure.
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
