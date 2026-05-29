#' @title **NHL Records - Venue Listing**
#' @description Returns the NHL arena/venue listing from the NHL Records API
#'   (`https://records.nhl.com/site/api/venue`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name            |types     |description                                  |
#'    |:-------------------|:---------|:--------------------------------------------|
#'    |id                  |integer   |Unique venue record identifier.              |
#'    |building_id         |integer   |Building identifier for the venue.           |
#'    |city                |character |City where the venue is located.             |
#'    |country_code        |character |Country code of the venue.                   |
#'    |outdoor             |logical   |Whether the venue is an outdoor rink.        |
#'    |postal_code         |character |Postal code of the venue.                    |
#'    |short_country_code  |character |Short country code of the venue.             |
#'    |state_province_code |character |State or province code of the venue.         |
#'    |venue_id            |integer   |Unique venue identifier.                     |
#'    |venue_name          |character |Name of the venue.                           |
#' @keywords NHL Records Venue
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_venue())
#' }
nhl_records_venue <- function(cayenne_exp = NULL) {
    raw <- .nhl_records_api(
        resource = "venue",
        cayenne_exp = cayenne_exp
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No venue data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Venue", Sys.time())
}
