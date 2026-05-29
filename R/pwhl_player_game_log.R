#' @title  **PWHL Player Game Log**
#' @description Retrieves game-by-game statistics for a PWHL player in a given season.
#'
#' @param player_id Numeric player ID
#' @param season Season (YYYY) to pull the game log from, the concluding year in XXXX-YY format.
#'   Defaults to `most_recent_pwhl_season()`.
#' @param game_type Game type: `"both"` (default), `"regular"`, `"playoffs"`,
#'   or `"preseason"`. When `"both"`, the player's regular-season and playoff
#'   game logs are combined and labeled by a `game_type` column. Game types
#'   the player has no games in (e.g. playoffs for a non-qualifying team) are
#'   omitted rather than erroring.
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
#'   * `game_type` - Game type the row belongs to (`"regular"`/`"playoffs"`).
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_player_game_log(player_id = 28, season = 2025))
#' }

pwhl_player_game_log <- function(player_id, season = most_recent_pwhl_season(), game_type = "both") {

  game_type <- match.arg(
    game_type,
    choices = c("both", "regular", "playoffs", "preseason")
  )

  # "both" expands to regular + playoffs (preseason intentionally excluded).
  game_types <- if (game_type == "both") c("regular", "playoffs") else game_type

  game_log <- data.frame()

  for (gt in game_types) {
    df <- .pwhl_player_game_log_one(
      player_id = player_id,
      season    = season,
      game_type = gt
    )
    if (!is.null(df) && nrow(df) > 0) {
      df$game_type <- gt
      game_log <- dplyr::bind_rows(game_log, df)
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
}

#' Fetch a PWHL player's game log for a single game type.
#'
#' Returns the cleaned per-game data frame for one resolved season/game_type,
#' or NULL when the game type does not exist for the season or the player has
#' no games in it. Errors degrade to NULL so a combined ("both") call survives
#' a game type the player did not appear in.
#'
#' @param player_id Numeric player ID.
#' @param season Numeric season (concluding) year.
#' @param game_type Character: "regular", "playoffs", or "preseason".
#' @return A data frame, or NULL.
#' @keywords internal
#' @noRd
.pwhl_player_game_log_one <- function(player_id, season, game_type) {
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

      if (nrow(game_log) == 0) {
        return(NULL)
      }

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

      return(game_log)
    },
    error = function(e) {
      return(NULL)
    }
  )
}
