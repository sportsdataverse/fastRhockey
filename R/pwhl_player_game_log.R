#' @title  **PWHL Player Game Log**
#' @description Retrieves game-by-game statistics for a PWHL player in a given season.
#'
#' @param player_id Numeric player ID
#' @param season Season (YYYY) to pull the game log from, the concluding year in XXXX-YY format.
#'   Defaults to `most_recent_pwhl_season()`.
#' @param game_type Game type: "regular" (default), "preseason", or "playoffs".
#' @return A data frame with per-game statistics for the player, or NULL if unavailable.
#'
#'   * `player_id` - Player ID.
#'   * `game_id` - Game ID.
#'   * `date` - Game date.
#'   * `team` - Team abbreviation.
#'   * `opponent` - Opponent abbreviation.
#'   * `home_away` - Home or Away indicator.
#'   * `goals` - Goals scored.
#'   * `assists` - Assists.
#'   * `points` - Total points.
#'   * `plus_minus` - Plus/minus rating.
#'   * `penalty_minutes` - Penalty minutes.
#'   * `shots` - Shots on goal.
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_player_game_log(player_id = 28, season = 2025))
#' }

pwhl_player_game_log <- function(player_id, season = most_recent_pwhl_season(), game_type = "regular") {

  tryCatch(
    expr = {
      season_id <- .pwhl_resolve_season_id(season = season, game_type = game_type)

      url <- .pwhl_modulekit_url(list(
        view = "player",
        category = "gamebygame",
        player_id = player_id,
        season_id = season_id
      ))

      r <- .pwhl_api(url)

      games_raw <- r$SiteKit$Player$games

      game_log <- data.frame()

      for (i in seq_along(games_raw)) {
        g <- games_raw[[i]]
        if (!is.null(g)) {
          game_row <- as.data.frame(
            lapply(g, function(x) if (is.null(x)) NA else as.character(x)),
            stringsAsFactors = FALSE
          )
          game_row$player_id <- as.numeric(player_id)
          game_log <- dplyr::bind_rows(game_log, game_row)
        }
      }

      if (nrow(game_log) > 0) {
        game_log <- game_log %>%
          janitor::clean_names()

        # Convert numeric columns
        num_cols <- intersect(
          names(game_log),
          c("id", "goals", "assists", "points", "plus_minus", "penalty_minutes",
            "shots", "goals_against", "saves", "shots_against", "win", "loss",
            "home", "goalie")
        )
        for (col in num_cols) {
          game_log[[col]] <- suppressWarnings(as.numeric(game_log[[col]]))
        }
      }

      if (nrow(game_log) == 0) {
        message(glue::glue("{Sys.time()}: No game log data found for player_id={player_id}, season={season}."))
        return(NULL)
      }

      game_log <- make_fastRhockey_data(
        game_log,
        type = "PWHL Player Game Log",
        timestamp = Sys.time()
      )

      return(game_log)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Error retrieving game log for player_id={player_id}. {e$message}"))
      return(NULL)
    }
  )
}
