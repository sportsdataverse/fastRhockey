#' @title **NHL Records - Goalie Season Stats**
#' @description Returns season-by-season goalie statistics from the NHL
#'   Records API
#'   (`https://records.nhl.com/site/api/goalie-season-stats`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @param limit Optional integer page size.
#' @param start Optional integer pagination offset.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                  |types     |description                                  |
#'    |:-------------------------|:---------|:--------------------------------------------|
#'    |id                        |integer   |Unique record identifier.                    |
#'    |active_player             |logical   |Indicator of whether the player is active.   |
#'    |first_name                |character |Player first name.                           |
#'    |franchise_id              |integer   |Unique franchise identifier.                 |
#'    |game_seven_games_played   |logical   |Game seven games played.                     |
#'    |game_seven_losses         |logical   |Game seven losses.                           |
#'    |game_seven_wins           |logical   |Game seven wins.                             |
#'    |game_type                 |integer   |Game type the totals belong to.             |
#'    |games_played              |integer   |Total games played.                          |
#'    |games_started             |integer   |Games started.                               |
#'    |goals_against             |integer   |Goals against.                               |
#'    |goals_against_average     |numeric   |Goals against average.                       |
#'    |last_name                 |character |Player last name.                            |
#'    |losses                    |integer   |Total losses.                                |
#'    |number_of_games_in_season |integer   |Number of games in the season.               |
#'    |overtime_games_played     |integer   |Overtime games played.                       |
#'    |overtime_goals_against    |integer   |Overtime goals against.                      |
#'    |overtime_losses           |logical   |Overtime losses.                             |
#'    |overtime_ties             |integer   |Overtime ties.                               |
#'    |overtime_wins             |integer   |Overtime wins.                               |
#'    |player_id                 |integer   |Unique player identifier.                    |
#'    |position_code             |character |Player position code.                        |
#'    |rookie_flag               |logical   |Indicator of whether the player was a rookie.|
#'    |save_pctg                 |numeric   |Save percentage.                             |
#'    |saves                     |integer   |Saves made.                                  |
#'    |season_id                 |integer   |Season identifier.                           |
#'    |shots_against             |integer   |Shots faced.                                 |
#'    |shutouts                  |integer   |Shutouts recorded.                           |
#'    |team_abbrevs              |character |Team abbreviations.                          |
#'    |team_names                |character |Team names.                                  |
#'    |ties                      |integer   |Total ties.                                  |
#'    |time_on_ice               |integer   |Total time on ice (seconds).                 |
#'    |time_on_ice_min_sec       |character |Total time on ice (MM:SS).                   |
#'    |wins                      |integer   |Total wins.                                  |
#' @keywords NHL Records Goalie Season Stats
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_goalie_season_stats(limit = 5))
#' }
nhl_records_goalie_season_stats <- function(
    cayenne_exp = NULL,
    limit = NULL,
    start = NULL
) {
    raw <- .nhl_records_api(
        resource = "goalie-season-stats",
        cayenne_exp = cayenne_exp,
        limit = limit,
        start = start
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No goalie season stats data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(
        df,
        "NHL Records Goalie Season Stats",
        Sys.time()
    )
}
