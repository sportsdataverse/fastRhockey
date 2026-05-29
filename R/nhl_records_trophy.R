#' @title **NHL Records - Trophy Listing**
#' @description Returns the NHL trophy listing from the NHL Records API
#'   (`https://records.nhl.com/site/api/trophy`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name          |types     |description                              |
#'    |:-----------------|:---------|:----------------------------------------|
#'    |id                |integer   |Unique trophy identifier.                |
#'    |brief_description |character |Brief description of the trophy.         |
#'    |category_id       |integer   |Trophy category identifier.              |
#'    |created_on        |character |Date the trophy record was created.      |
#'    |description       |character |Full description of the trophy.          |
#'    |footnote          |logical   |Footnote associated with the trophy.     |
#'    |home_page_url     |character |URL to the trophy's home page.           |
#'    |image_url         |character |URL to the trophy's image.               |
#'    |name              |character |Full name of the trophy.                 |
#'    |short_name        |character |Short name of the trophy.                |
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
