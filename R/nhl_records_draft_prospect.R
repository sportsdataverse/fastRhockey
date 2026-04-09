#' @title **NHL Records - Draft Prospect Listing**
#' @description Returns the draft prospect listing from the NHL Records API
#'   (`https://records.nhl.com/site/api/draft-prospect`). Optionally filter
#'   to a single prospect via `prospect_id` (switches resource to
#'   `draft-prospect/{prospect_id}`).
#' @param prospect_id Optional integer prospect ID. If supplied, the resource
#'   becomes `draft-prospect/{prospect_id}`.
#' @param cayenne_exp Optional Cayenne filter expression string. Ignored when
#'   `prospect_id` is supplied.
#' @return A `fastRhockey_data` tibble of draft prospects, or `NULL` on failure.
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
