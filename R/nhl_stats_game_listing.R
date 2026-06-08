#' @title **NHL Stats API — Game Listing**
#' @description Returns the game listing from the NHL Stats REST API
#'   (`https://api.nhle.com/stats/rest/{lang}/game`). Supports server-side
#'   filtering via a Cayenne expression.
#' @param lang Character language code. Default `"en"`.
#' @param limit Integer maximum number of results. Default 100.
#' @param start Integer pagination start index. Default 0.
#' @param cayenne_exp Optional Cayenne filter expression string passed via
#'   the `cayenneExp` query parameter (e.g., `"season=20242025"`).
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                |types     |description                          |
#'    |:-----------------------|:---------|:------------------------------------|
#'    |id                      |integer   |Unique game identifier.              |
#'    |eastern_start_time      |character |Game start time in Eastern time.     |
#'    |game_date               |character |Game date.                           |
#'    |game_number             |integer   |Game number within the schedule.     |
#'    |game_schedule_state_id  |integer   |Schedule state identifier.           |
#'    |game_state_id           |integer   |Game state identifier.               |
#'    |game_type               |integer   |Game type identifier.                |
#'    |home_score              |integer   |Home team score.                     |
#'    |home_team_id            |integer   |Home team identifier.                |
#'    |period                  |integer   |Period reached in the game.          |
#'    |season                  |integer   |Season (concluding year, YYYY).      |
#'    |visiting_score          |integer   |Visiting team score.                 |
#'    |visiting_team_id        |integer   |Visiting team identifier.            |
#' @keywords NHL Stats Game Listing
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_game_listing())
#' }
nhl_stats_game_listing <- function(
    lang = "en",
    limit = 100,
    start = 0,
    cayenne_exp = NULL
) {
    base_url <- glue::glue(
        "https://api.nhle.com/stats/rest/{lang}/game"
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
                message(glue::glue("{Sys.time()}: No game listing data"))
                return(NULL)
            }

            df <- raw$data %>%
                janitor::clean_names() %>%
                make_fastRhockey_data(
                    "NHL Stats Game Listing",
                    Sys.time()
                )

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching game listing: {e$message}"
            ))
            return(NULL)
        }
    )
}
