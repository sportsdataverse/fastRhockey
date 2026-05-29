#' @title **NHL Records - Draft Combine Measurements**
#' @description Returns NHL Scouting Combine measurements from the NHL
#'   Records API (`https://records.nhl.com/site/api/combine`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name          |types     |description                              |
#'    |:-----------------|:---------|:----------------------------------------|
#'    |id                |integer   |Unique combine record identifier.        |
#'    |amateur_club_name |character |Amateur club the player played for.      |
#'    |amateur_league    |character |Amateur league the player played in.     |
#'    |draft_year        |integer   |Draft year for the player.               |
#'    |event             |character |Combine measurement event name.          |
#'    |first_name        |character |Player first name.                       |
#'    |last_name         |character |Player last name.                        |
#'    |player_id         |integer   |Unique player identifier.                |
#'    |position_code     |character |Player position code.                    |
#'    |value             |numeric   |Measured value for the combine event.    |
#' @keywords NHL Records Combine
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_combine())
#' }
nhl_records_combine <- function(cayenne_exp = NULL) {
    raw <- .nhl_records_api(
        resource = "combine",
        cayenne_exp = cayenne_exp
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No combine data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Combine", Sys.time())
}
