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
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                  |types     |description                                       |
#'    |:-------------------------|:---------|:-------------------------------------------------|
#'    |g_month                   |character |Month the game was played.                        |
#'    |id                        |numeric   |Unique game identifier.                           |
#'    |home_team                 |character |Home team identifier.                             |
#'    |visiting_team             |character |Visiting team identifier.                         |
#'    |date_played               |character |Date the game was played.                         |
#'    |home                      |numeric   |Whether the player's team was home.               |
#'    |goalie                    |numeric   |Whether the player was the goalie.                |
#'    |home_team_code            |character |Home team abbreviation.                           |
#'    |home_team_name            |character |Home team name.                                   |
#'    |home_division             |character |Home team division.                               |
#'    |visiting_team_code        |character |Visiting team abbreviation.                       |
#'    |visiting_team_name        |character |Visiting team name.                               |
#'    |visiting_division         |character |Visiting team division.                           |
#'    |goals_against             |numeric   |Goals against (goalie).                           |
#'    |seconds_played            |character |Seconds played in the game.                       |
#'    |win                       |numeric   |Whether the game was a win (goalie).              |
#'    |tie                       |character |Whether the game was a tie (goalie).              |
#'    |loss                      |numeric   |Whether the game was a loss (goalie).             |
#'    |total_losses              |character |Total losses to date (goalie).                    |
#'    |shutout                   |character |Whether the game was a shutout (goalie).          |
#'    |ot_loss                   |character |Whether the game was an overtime loss.            |
#'    |shootout_loss             |character |Whether the game was a shootout loss.             |
#'    |saves                     |numeric   |Saves made (goalie).                              |
#'    |shots_against             |numeric   |Shots faced (goalie).                             |
#'    |shootout_saves            |character |Shootout saves made.                              |
#'    |shootout_goals_against    |character |Shootout goals against.                           |
#'    |shootout_shots            |character |Shootout shots faced.                             |
#'    |goals                     |numeric   |Goals scored.                                     |
#'    |assists                   |numeric   |Assists.                                          |
#'    |pim                       |character |Penalty minutes.                                  |
#'    |points                    |numeric   |Total points (goals + assists).                   |
#'    |gaa                       |character |Goals against average (goalie).                   |
#'    |svpct                     |character |Save percentage (goalie).                         |
#'    |shootout_shots_percentage |character |Shootout save percentage.                         |
#'    |penalty_minutes           |numeric   |Penalty minutes.                                  |
#'    |minutes                   |character |Minutes played.                                   |
#'    |player_team               |character |The player's team.                                |
#'    |player_id                 |numeric   |Unique player identifier.                         |
#'    |game_type                 |character |Game type the row belongs to ("regular"/"playoffs").|
#' @import jsonlite
#' @import dplyr
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
