#' @title **NHL Records - Draft Lottery Picks**
#' @description Returns historical NHL draft lottery picks/results from the
#'   NHL Records API
#'   (`https://records.nhl.com/site/api/draft-lottery-picks`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @return A `fastRhockey_data` tibble of draft lottery picks, or `NULL`
#'   on failure.
#' @keywords NHL Records Draft Lottery Picks
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_draft_lottery_picks())
#' }
nhl_records_draft_lottery_picks <- function(cayenne_exp = NULL) {
    raw <- .nhl_records_api(
        resource = "draft-lottery-picks",
        cayenne_exp = cayenne_exp
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No draft lottery picks data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(
        df,
        "NHL Records Draft Lottery Picks",
        Sys.time()
    )
}
