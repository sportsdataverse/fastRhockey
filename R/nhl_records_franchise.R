#' @title **NHL Records - Franchise Listing**
#' @description Returns the franchise listing from the NHL Records API
#'   (`https://records.nhl.com/site/api/franchise`). Optionally filter to a
#'   single franchise via `franchise_id`, which is translated into the
#'   Cayenne filter `id={franchise_id}` (the records API does **not**
#'   accept a path-suffix `franchise/{id}` form — it returns 404).
#' @param franchise_id Optional integer franchise ID.
#' @param lang Character language code. Default `"en"`. Currently unused
#'   server-side but reserved for future use.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name            |types     |description                              |
#'    |:-------------------|:---------|:----------------------------------------|
#'    |id                  |integer   |Unique franchise identifier.             |
#'    |first_season_id     |integer   |Season identifier of the first season.   |
#'    |full_name           |character |Full franchise name.                     |
#'    |last_season_id      |integer   |Season identifier of the last season.    |
#'    |most_recent_team_id |integer   |Most recent team identifier.             |
#'    |team_abbrev         |character |Team abbreviation.                       |
#'    |team_common_name    |character |Team common (nickname) name.             |
#'    |team_place_name     |character |Team place (location) name.              |
#' @keywords NHL Records Franchise
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_franchise())
#'   try(nhl_records_franchise(franchise_id = 5))
#' }
nhl_records_franchise <- function(franchise_id = NULL, lang = "en") {
    cayenne <- if (!is.null(franchise_id)) {
        glue::glue("id={as.integer(franchise_id)}")
    } else {
        NULL
    }
    raw <- .nhl_records_api(resource = "franchise", cayenne_exp = cayenne)
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No franchise data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Franchise", Sys.time())
}
