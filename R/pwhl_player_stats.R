#' @title  **PWHL Player Season Stats**
#' @description Retrieves career and season-by-season statistics for a PWHL player.
#'
#' @param player_id Numeric player ID
#' @return A data frame with per-season statistics for the player, or NULL if unavailable.
#'
#'   * `player_id` - Player ID.
#'   * `season_name` - Season name.
#'   * `season_id` - Season ID.
#'   * `team` - Team abbreviation.
#'   * `league` - League name.
#'   * `games_played` - Games played.
#'   * `goals` - Goals scored.
#'   * `assists` - Assists.
#'   * `points` - Total points.
#'   * `plus_minus` - Plus/minus rating.
#'   * `penalty_minutes` - Penalty minutes.
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_player_stats(player_id = 28))
#' }

pwhl_player_stats <- function(player_id) {

  tryCatch(
    expr = {
      url <- .pwhl_modulekit_url(list(
        view = "player",
        category = "seasonstats",
        player_id = player_id
      ))

      r <- .pwhl_api(url)

      player_data <- r$SiteKit$Player

      stats <- data.frame()

      # Process regular season and playoff stats
      for (stat_type in c("regular", "playoff")) {
        seasons_raw <- player_data[[stat_type]]
        if (!is.null(seasons_raw)) {
          for (i in seq_along(seasons_raw)) {
            s <- seasons_raw[[i]]
            if (!is.null(s)) {
              stat_row <- as.data.frame(
                lapply(s, function(x) if (is.null(x)) NA else as.character(x)),
                stringsAsFactors = FALSE
              )
              stat_row$player_id <- as.numeric(player_id)
              stat_row$stat_type <- stat_type
              stats <- dplyr::bind_rows(stats, stat_row)
            }
          }
        }
      }

      if (nrow(stats) == 0) {
        message(glue::glue("{Sys.time()}: No season stats found for player_id={player_id}."))
        return(NULL)
      }

      stats <- stats %>%
        janitor::clean_names()

      # Convert numeric columns
      num_cols <- intersect(
        names(stats),
        c("season_id", "games_played", "goals", "assists", "points", "plus_minus",
          "penalty_minutes", "shots", "goals_against", "goals_against_average",
          "minutes_played", "shots_against", "wins", "losses", "saves",
          "savepct", "shutouts")
      )
      for (col in num_cols) {
        stats[[col]] <- suppressWarnings(as.numeric(stats[[col]]))
      }

      stats <- make_fastRhockey_data(
        stats,
        type = "PWHL Player Season Stats",
        timestamp = Sys.time()
      )

      return(stats)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Error retrieving season stats for player_id={player_id}. {e$message}"))
      return(NULL)
    }
  )
}
