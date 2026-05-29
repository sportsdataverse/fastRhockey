#' @title **NHL Records - Skater Real-Time Stats by Season**
#' @description Returns season-by-season skater real-time stats (hits,
#'   giveaways, takeaways, blocks, faceoffs, time on ice) from the NHL
#'   Records API
#'   (`https://records.nhl.com/site/api/skater-real-time-stats-season`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @param limit Optional integer page size.
#' @param start Optional integer pagination offset.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name           |types     |description                                  |
#'    |:------------------|:---------|:--------------------------------------------|
#'    |id                 |integer   |Unique record identifier.                    |
#'    |active_player      |logical   |Whether the player is currently active.      |
#'    |blocked_shots      |integer   |Blocked shots in the season.                 |
#'    |faceoff_win_pctg   |logical   |Faceoff win percentage for the season.       |
#'    |faceoffs_lost      |integer   |Faceoffs lost in the season.                 |
#'    |faceoffs_taken     |integer   |Faceoffs taken in the season.                |
#'    |faceoffs_won       |integer   |Faceoffs won in the season.                  |
#'    |first_name         |character |First name of the player.                    |
#'    |franchise_id       |integer   |Franchise identifier.                        |
#'    |game_type_id       |integer   |Game type identifier (regular/playoffs).     |
#'    |games_in_schedule  |integer   |Games in the schedule.                       |
#'    |games_played       |integer   |Games played in the season.                  |
#'    |giveaways          |integer   |Giveaways in the season.                     |
#'    |hits               |integer   |Hits in the season.                          |
#'    |last_name          |character |Last name of the player.                     |
#'    |missed_shots       |integer   |Missed shots in the season.                  |
#'    |player_id          |integer   |Unique player identifier.                    |
#'    |position_code      |character |Player position code.                        |
#'    |rookie_flag        |logical   |Whether the season was a rookie season.      |
#'    |season_id          |integer   |Season identifier.                           |
#'    |shifts             |integer   |Shifts in the season.                        |
#'    |takeaways          |integer   |Takeaways in the season.                     |
#'    |team_abbrevs       |character |Team abbreviations for the season.           |
#'    |team_names         |character |Team names for the season.                   |
#'    |time_on_ice        |integer   |Time on ice in the season.                   |
#' @keywords NHL Records Skater Real Time Season
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_skater_real_time_stats_season(limit = 5))
#' }
nhl_records_skater_real_time_stats_season <- function(
    cayenne_exp = NULL,
    limit = NULL,
    start = NULL
) {
    raw <- .nhl_records_api(
        resource = "skater-real-time-stats-season",
        cayenne_exp = cayenne_exp,
        limit = limit,
        start = start
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf(
            "%s: No skater real-time season stats data",
            Sys.time()
        ))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(
        df,
        "NHL Records Skater Real-Time Stats Season",
        Sys.time()
    )
}
