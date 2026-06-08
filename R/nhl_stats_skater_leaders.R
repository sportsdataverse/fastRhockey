#' @title **NHL Stats API — Skater Leaders**
#' @description Returns a skater leaderboard for a given statistic attribute
#'   from the NHL Stats REST API
#'   (`https://api.nhle.com/stats/rest/{lang}/leaders/skaters/{attribute}`).
#'   Distinct from the api-web `skater-stats-leaders` endpoint.
#' @param attribute Character (required). The stat attribute to rank by.
#'   Known valid values include `"points"`, `"goals"`, `"assists"`. Note
#'   that this endpoint does **not** accept `start` / `limit` query
#'   parameters — passing them produces a 500. Use `cayenne_exp` to filter.
#' @param lang Character language code. Default `"en"`.
#' @param cayenne_exp Optional Cayenne filter expression string passed via
#'   the `cayenneExp` query parameter (e.g., `"seasonId=20242025"`).
#' @return A data frame (`fastRhockey_data`) of skater leaders, or `NULL` on
#'   failure. When ranking by `"assists"` the columns are:
#'
#'    |col_name                |types     |description                                  |
#'    |:-----------------------|:---------|:--------------------------------------------|
#'    |assists                 |integer   |Assists (the ranked attribute).              |
#'    |player_id               |integer   |Unique player identifier.                    |
#'    |player_current_team_id  |logical   |Player's current team identifier.            |
#'    |player_first_name       |character |Player first name.                           |
#'    |player_full_name        |character |Player full name.                            |
#'    |player_last_name        |character |Player last name.                            |
#'    |player_position_code    |character |Player position code.                        |
#'    |player_sweater_number   |integer   |Player jersey number.                        |
#'    |team_id                 |integer   |Unique team identifier.                      |
#'    |team_franchise_id       |integer   |Team franchise identifier.                   |
#'    |team_full_name          |character |Team full name.                              |
#'    |team_league_id          |integer   |League identifier of the team.               |
#'    |team_logos              |list      |Team logo metadata.                          |
#'    |team_raw_tricode        |character |Team raw three-letter code.                  |
#'    |team_tri_code           |character |Team three-letter code.                      |
#' @keywords NHL Stats Skater Leaders
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_skater_leaders(attribute = "assists"))
#' }
nhl_stats_skater_leaders <- function(
    attribute,
    lang = "en",
    cayenne_exp = NULL
) {
    url <- glue::glue(
        "https://api.nhle.com/stats/rest/{lang}/leaders/skaters/{attribute}"
    )
    if (!is.null(cayenne_exp)) {
        url <- paste0(
            url,
            "?cayenneExp=",
            utils::URLencode(cayenne_exp, reserved = TRUE)
        )
    }

    tryCatch(
        expr = {
            res <- .retry_request(url)
            check_status(res)

            resp_text <- .resp_text(res)
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            if (
                is.null(raw$data) ||
                    (is.data.frame(raw$data) && nrow(raw$data) == 0)
            ) {
                message(glue::glue(
                    "{Sys.time()}: No skater leaders data for '{attribute}'"
                ))
                return(NULL)
            }

            df <- raw$data %>%
                janitor::clean_names() %>%
                make_fastRhockey_data(
                    "NHL Stats Skater Leaders",
                    Sys.time()
                )

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching skater leaders: {e$message}"
            ))
            return(NULL)
        }
    )
}
