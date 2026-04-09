#' @title **NHL Records - Trophy Listing**
#' @description Returns the NHL trophy listing from the NHL Records API
#'   (`https://records.nhl.com/site/api/trophy`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @return A `fastRhockey_data` tibble of trophies, or `NULL` on failure.
#' @keywords NHL Records Trophy
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_trophy())
#' }
nhl_records_trophy <- function(cayenne_exp = NULL) {
    raw <- .nhl_records_api(
        resource = "trophy",
        cayenne_exp = cayenne_exp
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No trophy data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Trophy", Sys.time())
}
