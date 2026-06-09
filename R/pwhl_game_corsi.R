#' @name pwhl_game_corsi
NULL
#' @title **PWHL Game Corsi/Fenwick (player-level on-ice)**
#' @description Player-level on-ice shot-attempt metrics for a PWHL game.
#'   Corsi = shot + blocked_shot + goal; Fenwick excludes blocked shots.
#'   Missed shots are not present in the HockeyTech feed, so both are proxies
#'   (`corsi_includes_missed = FALSE`). TOI is left-joined from
#'   [pwhl_game_shifts()] so every on-ice player is retained even when shift
#'   data is absent (resulting in `NA` `toi_seconds`).
#' @rdname pwhl_game_corsi
#' @author Saiem Gilani
#' @param game_id PWHL game id (integer or character).
#' @return A `fastRhockey_data` data frame with one row per on-ice player:
#'
#'    |col_name              |types     |description                                                  |
#'    |:---------------------|:---------|:------------------------------------------------------------|
#'    |player_id             |character |Player unique identifier.                                    |
#'    |corsi_for             |integer   |On-ice shot attempts for (shot + blocked_shot + goal).       |
#'    |corsi_against         |integer   |On-ice shot attempts against.                                |
#'    |corsi_for_pct         |numeric   |Corsi for percentage (NA when total = 0).                    |
#'    |fenwick_for           |integer   |On-ice unblocked shot attempts for (shot + goal).            |
#'    |fenwick_against       |integer   |On-ice unblocked shot attempts against.                      |
#'    |fenwick_for_pct       |numeric   |Fenwick for percentage (NA when total = 0).                  |
#'    |corsi_includes_missed |logical   |Always FALSE (no missed-shot events in the HockeyTech feed). |
#'    |toi_seconds           |integer   |Total time on ice in seconds (NA if not in shift data).      |
#'    |corsi_for_per60       |numeric   |Corsi For per 60 minutes (NA when toi_seconds is 0 or NA).  |
#'
#' @import dplyr
#' @export
#' @family PWHL Functions
#' @examples
#' \donttest{
#'   try(pwhl_game_corsi(game_id = 42))
#' }
pwhl_game_corsi <- function(game_id) {
  corsi_df <- data.frame()
  tryCatch(
    expr = {
      pbp   <- pwhl_pbp(game_id = game_id)
      corsi <- hockeytech_corsi_fenwick_on_ice(pbp)

      # Coerce join key to character for safe matching across id types
      corsi$player_id <- as.character(corsi$player_id)

      # TOI join: wrap in its own tryCatch so a shift-feed failure degrades
      # gracefully -- corsi is still returned with toi_seconds/corsi_for_per60 = NA.
      toi <- tryCatch(
        {
          t <- hockeytech_player_toi(pwhl_game_shifts(game_id = game_id))
          t$player_id <- as.character(t$player_id)
          t
        },
        error = function(e) {
          cli::cli_alert_warning(
            "{Sys.time()}: TOI fetch failed for game_id {game_id}; corsi_for_per60 will be NA. {e}"
          )
          NULL
        }
      )

      if (!is.null(toi) && nrow(toi) > 0) {
        corsi <- dplyr::left_join(
          corsi,
          toi[, c("player_id", "toi_seconds")],
          by = "player_id"
        )
      } else {
        corsi$toi_seconds <- NA_integer_
      }

      # Per-60 rate; guard divide-by-zero / NA toi_seconds
      corsi$corsi_for_per60 <- ifelse(
        !is.na(corsi$toi_seconds) & corsi$toi_seconds > 0,
        hockeytech_per60(corsi$corsi_for, corsi$toi_seconds),
        NA_real_
      )

      corsi_df <- make_fastRhockey_data(
        corsi,
        "PWHL Game Corsi from HockeyTech",
        Sys.time()
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "{Sys.time()}: game_id {game_id} unavailable! {e}"
      )
    }
  )
  corsi_df
}
