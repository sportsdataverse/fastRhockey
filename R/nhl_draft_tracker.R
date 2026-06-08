#' @title **NHL Draft Tracker**
#' @description Returns the live (real-time) NHL draft tracker picks from the
#' NHL web service endpoint `draft-tracker/picks/now`.
#'
#' This is distinct from [`nhl_draft()`], which hits `draft/picks/now` and
#' returns the static draft board. The draft tracker is only populated during
#' an active NHL draft window and will return `NULL` (or an empty payload)
#' outside of that window.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                                 |types     |description                                  |
#'    |:----------------------------------------|:---------|:--------------------------------------------|
#'    |pick_in_round                            |integer   |Pick number within the round.                |
#'    |overall_pick                             |integer   |Overall pick number in the draft.            |
#'    |team_id                                  |integer   |Unique team identifier.                      |
#'    |team_abbrev                              |character |Team abbreviation.                           |
#'    |team_logo_light                          |character |URL to the team logo (light variant).        |
#'    |team_logo_dark                           |character |URL to the team logo (dark variant).         |
#'    |state                                    |character |Pick state (e.g., on the clock, complete).   |
#'    |position_code                            |character |Player position code.                        |
#'    |team_full_name_default                   |character |Team full name (default language).           |
#'    |team_full_name_fr                        |character |Team full name (French).                     |
#'    |team_common_name_default                 |character |Team common name (default language).         |
#'    |team_common_name_fr                      |character |Team common name (French).                   |
#'    |team_place_name_with_preposition_default |character |Team place name with preposition (default).  |
#'    |team_place_name_with_preposition_fr      |character |Team place name with preposition (French).   |
#'    |last_name_default                        |character |Player last name (default language).         |
#'    |first_name_default                       |character |Player first name (default language).        |
#' @keywords NHL Draft Tracker
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_draft_tracker())
#' }
nhl_draft_tracker <- function() {
    url <- "https://api-web.nhle.com/v1/draft-tracker/picks/now"

    tryCatch(
        expr = {
            res <- .retry_request(url)
            check_status(res)

            resp_text <- .resp_text(res)
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            picks <- NULL
            if (is.data.frame(raw)) {
                picks <- raw
            } else if (is.list(raw) && !is.null(raw[["picks"]])) {
                picks <- raw[["picks"]]
            }

            if (is.null(picks) ||
                (is.data.frame(picks) && nrow(picks) == 0) ||
                length(picks) == 0) {
                message(glue::glue(
                    "{Sys.time()}: No active NHL draft tracker data available!"
                ))
                return(NULL)
            }

            picks_df <- as.data.frame(picks) %>%
                janitor::clean_names() %>%
                make_fastRhockey_data(
                    "NHL Draft Tracker data from NHL.com",
                    Sys.time()
                )
            return(picks_df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching NHL draft tracker: {e$message}"
            ))
            return(NULL)
        }
    )
}
