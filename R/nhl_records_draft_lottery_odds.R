#' @title **NHL Records - Draft Lottery Odds**
#' @description Returns NHL draft lottery odds from the NHL Records API
#'   (`https://records.nhl.com/site/api/draft-lottery-odds`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name        |types     |description                              |
#'    |:---------------|:---------|:----------------------------------------|
#'    |id              |integer   |Unique lottery odds record identifier.   |
#'    |draft_year      |integer   |Draft year the odds apply to.            |
#'    |format_content  |character |Description of the lottery format.       |
#'    |odds_content    |character |Description of the lottery odds.         |
#'    |result_notes    |character |Notes on the lottery results.            |
#' @keywords NHL Records Draft Lottery Odds
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_draft_lottery_odds())
#' }
nhl_records_draft_lottery_odds <- function(cayenne_exp = NULL) {
    raw <- .nhl_records_api(
        resource = "draft-lottery-odds",
        cayenne_exp = cayenne_exp
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No draft lottery odds data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Draft Lottery Odds", Sys.time())
}
