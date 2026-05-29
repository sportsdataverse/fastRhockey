#' @title **NHL Stats API — Goalie Milestones**
#' @description Returns goalie milestone achievements from the NHL Stats
#'   REST API (`https://api.nhle.com/stats/rest/{lang}/milestones/goalies`).
#' @param lang Character language code. Default `"en"`.
#' @param cayenne_exp Optional Cayenne filter expression string passed via
#'   the `cayenneExp` query parameter.
#' @param limit Integer maximum number of results. Default 100.
#' @param start Integer pagination start index. Default 0.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name           |types     |description                             |
#'    |:------------------|:---------|:---------------------------------------|
#'    |id                 |integer   |Unique milestone record identifier.     |
#'    |current_team_id    |integer   |Player's current team identifier.       |
#'    |first_name         |character |Player first name.                      |
#'    |game_type_id       |integer   |Game type identifier.                   |
#'    |games_played       |integer   |Games played.                           |
#'    |last_name          |character |Player last name.                       |
#'    |milestone          |character |Milestone category.                     |
#'    |milestone_amount   |integer   |Amount remaining to reach the milestone.|
#'    |player_full_name   |character |Player full name.                       |
#'    |player_id          |integer   |Unique player identifier.               |
#'    |so                 |integer   |Shutouts.                               |
#'    |team_abbrev        |character |Team abbreviation.                      |
#'    |team_common_name   |character |Team common (nickname) name.            |
#'    |team_full_name     |character |Full team name.                         |
#'    |team_place_name    |character |Team place (city/location) name.        |
#'    |toi_minutes        |integer   |Time on ice in minutes.                 |
#'    |wins               |integer   |Wins.                                   |
#' @keywords NHL Stats Goalie Milestones
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_goalie_milestones())
#' }
nhl_stats_goalie_milestones <- function(
    lang = "en",
    cayenne_exp = NULL,
    limit = 100,
    start = 0
) {
    base_url <- glue::glue(
        "https://api.nhle.com/stats/rest/{lang}/milestones/goalies"
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
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            if (
                is.null(raw$data) ||
                    (is.data.frame(raw$data) && nrow(raw$data) == 0)
            ) {
                message(glue::glue(
                    "{Sys.time()}: No goalie milestones data"
                ))
                return(NULL)
            }

            df <- raw$data %>%
                janitor::clean_names() %>%
                make_fastRhockey_data(
                    "NHL Stats Goalie Milestones",
                    Sys.time()
                )

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching goalie milestones: {e$message}"
            ))
            return(NULL)
        }
    )
}
