#' @title  **PWHL Player Season Stats**
#' @description Retrieves career and season-by-season statistics for a PWHL player.
#'
#' @param player_id Numeric player ID
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name               |types     |description                                  |
#'    |:----------------------|:---------|:--------------------------------------------|
#'    |season_id              |numeric   |Season identifier.                           |
#'    |season_name            |character |Season name.                                 |
#'    |shortname              |character |Short season name.                           |
#'    |playoff                |character |Whether the row is playoff statistics.       |
#'    |career                 |character |Whether the row is career totals.            |
#'    |max_start_date         |character |Latest game start date for the season.       |
#'    |veteran_status         |character |Player veteran status.                       |
#'    |veteran                |character |Whether the player is a veteran.             |
#'    |goals_against          |numeric   |Goals against (goalie).                      |
#'    |shootout_goals_against |character |Shootout goals against.                      |
#'    |shootout_saves         |character |Shootout saves made.                         |
#'    |goals_against_average  |numeric   |Goals against average (goalie).              |
#'    |games_played           |numeric   |Games played.                                |
#'    |minutes_played         |numeric   |Minutes played.                              |
#'    |seconds_played         |character |Seconds played.                              |
#'    |shots_against          |numeric   |Shots faced (goalie).                        |
#'    |wins                   |numeric   |Wins (goalie).                               |
#'    |losses                 |numeric   |Losses (goalie).                             |
#'    |ties                   |character |Ties (goalie).                               |
#'    |ot_losses              |character |Overtime losses (goalie).                    |
#'    |total_losses           |character |Total losses (goalie).                       |
#'    |shootout_losses        |character |Shootout losses (goalie).                    |
#'    |ot                     |character |Overtime results.                            |
#'    |sosavepct              |character |Shootout save percentage.                    |
#'    |shootout_shots         |character |Shootout shots faced.                        |
#'    |shutouts               |numeric   |Shutouts recorded (goalie).                  |
#'    |saves                  |numeric   |Saves made (goalie).                         |
#'    |savepct                |numeric   |Save percentage (goalie).                    |
#'    |goals                  |numeric   |Goals scored.                                |
#'    |assists                |numeric   |Assists.                                     |
#'    |points                 |numeric   |Total points (goals + assists).              |
#'    |penalty_minutes        |numeric   |Penalty minutes.                             |
#'    |team_name              |character |Team name.                                   |
#'    |team_code              |character |Team abbreviation.                           |
#'    |team_city              |character |Team city.                                   |
#'    |team_nickname          |character |Team nickname.                               |
#'    |team_id                |character |Unique team identifier.                      |
#'    |division               |character |Team division.                               |
#'    |shotspct               |character |Shooting percentage.                         |
#'    |gaa                    |character |Goals against average (goalie).              |
#'    |player_id              |numeric   |Unique player identifier.                    |
#'    |stat_type              |character |Statistic type ("regular"/"playoff").        |
#' @import jsonlite
#' @import dplyr
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
