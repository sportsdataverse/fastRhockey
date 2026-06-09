#' @name pwhl_player_toi
NULL
#' @title **PWHL Player Time On Ice**
#' @description Per-player time-on-ice totals for a PWHL game. Fetches all
#'   shift stints via [pwhl_game_shifts()] then aggregates them with the
#'   internal `hockeytech_player_toi()` helper.
#' @rdname pwhl_player_toi
#' @author Saiem Gilani
#' @param game_id PWHL game id (integer or character).
#' @return A `fastRhockey_data` data frame with one row per player, sorted
#'   descending by `toi_seconds`:
#'
#'    |col_name    |types     |description                                |
#'    |:-----------|:---------|:------------------------------------------|
#'    |player_id   |numeric   |Player unique identifier.                  |
#'    |first_name  |character |Player first name.                         |
#'    |last_name   |character |Player last name.                          |
#'    |toi_seconds |integer   |Total time on ice in seconds.              |
#'    |num_shifts  |integer   |Number of individual shift stints.         |
#'    |avg_shift_s |numeric   |Average shift length in seconds.           |
#'
#' @import dplyr
#' @export
#' @family PWHL Functions
#' @examples
#' \donttest{
#'   try(pwhl_player_toi(game_id = 42))
#' }
pwhl_player_toi <- function(game_id) {
  toi_df <- data.frame()
  tryCatch(
    expr = {
      toi_df <- hockeytech_player_toi(pwhl_game_shifts(game_id = game_id))
      toi_df <- make_fastRhockey_data(
        toi_df,
        "PWHL Player TOI from HockeyTech",
        Sys.time()
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "{Sys.time()}: game_id {game_id} unavailable! {e}"
      )
    }
  )
  toi_df
}
