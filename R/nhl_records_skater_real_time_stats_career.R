#' @title **NHL Records - Skater Real-Time Stats Career**
#' @description Returns career skater real-time stats (hits, giveaways,
#'   takeaways, blocks, faceoffs, time on ice) from the NHL Records API
#'   (`https://records.nhl.com/site/api/skater-real-time-stats-career`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @param limit Optional integer page size.
#' @param start Optional integer pagination offset.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                    |types     |description                                  |
#'    |:---------------------------|:---------|:--------------------------------------------|
#'    |id                          |integer   |Unique record identifier.                    |
#'    |active_player               |logical   |Whether the player is currently active.      |
#'    |blocked_shots               |integer   |Career blocked shots.                        |
#'    |faceoff_win_pctg            |logical   |Career faceoff win percentage.               |
#'    |faceoffs_lost               |integer   |Career faceoffs lost.                        |
#'    |faceoffs_taken              |integer   |Career faceoffs taken.                       |
#'    |faceoffs_won                |integer   |Career faceoffs won.                         |
#'    |first_name                  |character |First name of the player.                    |
#'    |first_season_for_game_type  |integer   |First season for the game type.              |
#'    |franchise_id                |integer   |Franchise identifier.                        |
#'    |game_type_id                |integer   |Game type identifier (regular/playoffs).     |
#'    |games_played                |integer   |Career games played.                         |
#'    |giveaways                   |integer   |Career giveaways.                            |
#'    |hits                        |integer   |Career hits.                                 |
#'    |last_name                   |character |Last name of the player.                     |
#'    |last_season_for_game_type   |integer   |Last season for the game type.               |
#'    |missed_shots                |integer   |Career missed shots.                         |
#'    |player_id                   |integer   |Unique player identifier.                    |
#'    |position_code               |character |Player position code.                        |
#'    |seasons_played              |integer   |Number of seasons played.                    |
#'    |shifts                      |integer   |Career shifts.                               |
#'    |takeaways                   |integer   |Career takeaways.                            |
#'    |team_abbrevs                |character |Team abbreviations the player suited up for. |
#'    |time_on_ice                 |integer   |Career time on ice.                          |
#' @keywords NHL Records Skater Real Time Career
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_skater_real_time_stats_career(limit = 5))
#' }
nhl_records_skater_real_time_stats_career <- function(
    cayenne_exp = NULL,
    limit = NULL,
    start = NULL
) {
    raw <- .nhl_records_api(
        resource = "skater-real-time-stats-career",
        cayenne_exp = cayenne_exp,
        limit = limit,
        start = start
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf(
            "%s: No skater real-time career stats data",
            Sys.time()
        ))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(
        df,
        "NHL Records Skater Real-Time Stats Career",
        Sys.time()
    )
}
