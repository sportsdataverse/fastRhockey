#' @title **NHL Records - Goalie Shutout Streaks**
#' @description Returns goalie shutout streak records from the NHL Records
#'   API (`https://records.nhl.com/site/api/goalie-shutout-streak`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @param limit Optional integer page size.
#' @param start Optional integer pagination offset.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name         |types     |description                                  |
#'    |:----------------|:---------|:--------------------------------------------|
#'    |id               |integer   |Unique record identifier.                    |
#'    |active_player    |logical   |Indicator of whether the player is active.   |
#'    |active_streak    |logical   |Indicator of whether the streak is active.   |
#'    |duration_min_sec |character |Streak duration (MM:SS).                     |
#'    |duration_seconds |integer   |Streak duration in seconds.                  |
#'    |end_date         |character |Date the streak ended.                       |
#'    |first_name       |character |Player first name.                           |
#'    |franchise_id     |integer   |Unique franchise identifier.                 |
#'    |game_type_id     |integer   |Game type the streak belongs to.             |
#'    |last_name        |character |Player last name.                            |
#'    |player_id        |integer   |Unique player identifier.                    |
#'    |saves            |logical   |Saves made during the streak.                |
#'    |season_id        |integer   |Season identifier.                           |
#'    |start_date       |character |Date the streak started.                     |
#'    |team_abbrev      |character |Team abbreviation.                           |
#'    |team_id          |integer   |Unique team identifier.                      |
#'    |team_name        |character |Team name.                                   |
#' @keywords NHL Records Goalie Shutout Streak
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_goalie_shutout_streak(limit = 5))
#' }
nhl_records_goalie_shutout_streak <- function(
    cayenne_exp = NULL,
    limit = NULL,
    start = NULL
) {
    raw <- .nhl_records_api(
        resource = "goalie-shutout-streak",
        cayenne_exp = cayenne_exp,
        limit = limit,
        start = start
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No goalie shutout streak data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(
        df,
        "NHL Records Goalie Shutout Streak",
        Sys.time()
    )
}
