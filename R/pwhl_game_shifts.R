#' @name pwhl_game_shifts
NULL
#' @title **PWHL Game Shifts**
#' @description All player shifts for a PWHL game (one row per stint), from
#'   HockeyTech. Calls the HockeyTech `modulekit/gameshifts` endpoint and
#'   returns one row per player-shift stint via the internal
#'   `.parse_hockeytech_shifts()` helper.
#' @rdname pwhl_game_shifts
#' @author Saiem Gilani
#' @param game_id PWHL game id (integer or character).
#' @return A `fastRhockey_data` data frame with one row per shift stint:
#'
#'    |col_name          |types     |description                                       |
#'    |:-----------------|:---------|:-------------------------------------------------|
#'    |game_id           |numeric   |Game identifier (echoed from argument).           |
#'    |player_id         |numeric   |Player unique identifier.                         |
#'    |first_name        |character |Player first name.                                |
#'    |last_name         |character |Player last name.                                 |
#'    |jersey_number     |character |Player jersey number.                             |
#'    |home              |integer   |1 if home player, 0 if visitor.                   |
#'    |period            |integer   |Period of the shift.                              |
#'    |start_time        |character |Shift start time (MM:SS countdown clock).         |
#'    |end_time          |character |Shift end time (MM:SS countdown clock).           |
#'    |length            |character |Shift length string.                              |
#'    |start_s           |integer   |Shift start in countdown seconds.                 |
#'    |end_s             |integer   |Shift end in countdown seconds.                   |
#'    |goal_on_shift     |integer   |1 if a goal occurred during this shift, else 0.   |
#'    |penalty_on_shift  |integer   |1 if a penalty occurred during this shift, else 0.|
#'
#' @import dplyr
#' @importFrom glue glue
#' @export
#' @family PWHL Functions
#' @examples
#' \donttest{
#'   try(pwhl_game_shifts(game_id = 42))
#' }
pwhl_game_shifts <- function(game_id) {
  shifts_df <- data.frame()
  tryCatch(
    expr = {
      payload   <- .hockeytech_api(
        .hockeytech_url("pwhl", "modulekit", "gameshifts", list(game_id = game_id))
      )
      shifts_df <- .parse_hockeytech_shifts(payload, game_id = game_id)
      shifts_df <- make_fastRhockey_data(
        shifts_df,
        "PWHL Game Shifts data from HockeyTech",
        Sys.time()
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "{Sys.time()}: game_id {game_id} unavailable! {e}"
      )
    }
  )
  shifts_df
}
