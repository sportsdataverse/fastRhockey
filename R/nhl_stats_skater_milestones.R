#' @title **NHL Stats API — Skater Milestones**
#' @description Returns skater milestone achievements from the NHL Stats
#'   REST API (`https://api.nhle.com/stats/rest/{lang}/milestones/skaters`).
#' @param lang Character language code. Default `"en"`.
#' @param cayenne_exp Optional Cayenne filter expression string passed via
#'   the `cayenneExp` query parameter.
#' @param limit Integer maximum number of results. Default 100.
#' @param start Integer pagination start index. Default 0.
#' @return A data frame (`fastRhockey_data`) of skater milestones, or `NULL`
#'   on failure, with the following columns:
#'
#'    |col_name          |types     |description                                          |
#'    |:-----------------|:---------|:----------------------------------------------------|
#'    |id                |integer   |Unique milestone record identifier.                  |
#'    |assists           |integer   |Assists.                                             |
#'    |current_team_id   |integer   |Player's current team identifier.                    |
#'    |first_name        |character |Player first name.                                   |
#'    |game_type_id      |integer   |Game type identifier (regular season / playoffs).    |
#'    |games_played      |integer   |Games played.                                        |
#'    |goals             |integer   |Goals scored.                                        |
#'    |last_name         |character |Player last name.                                    |
#'    |milestone         |character |Milestone category.                                  |
#'    |milestone_amount  |integer   |Milestone threshold amount.                          |
#'    |player_full_name  |character |Player full name.                                    |
#'    |player_id         |integer   |Unique player identifier.                            |
#'    |points            |integer   |Total points (goals + assists).                      |
#'    |team_abbrev       |character |Team abbreviation.                                   |
#'    |team_common_name  |character |Team common name.                                    |
#'    |team_full_name    |character |Team full name.                                      |
#'    |team_place_name   |character |Team place (city) name.                              |
#' @keywords NHL Stats Skater Milestones
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_skater_milestones())
#' }
nhl_stats_skater_milestones <- function(
    lang = "en",
    cayenne_exp = NULL,
    limit = 100,
    start = 0
) {
    base_url <- glue::glue(
        "https://api.nhle.com/stats/rest/{lang}/milestones/skaters"
    )
    url <- paste0(base_url, "?start=", start, "&limit=", limit)
    if (!is.null(cayenne_exp)) {
        url <- paste0(
            url,
            "&cayenneExp=",
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
                    "{Sys.time()}: No skater milestones data"
                ))
                return(NULL)
            }

            df <- raw$data %>%
                janitor::clean_names() %>%
                make_fastRhockey_data(
                    "NHL Stats Skater Milestones",
                    Sys.time()
                )

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching skater milestones: {e$message}"
            ))
            return(NULL)
        }
    )
}
