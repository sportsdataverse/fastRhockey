#' @title  **PWHL Player Streaks**
#' @description Retrieves player streak data for a PWHL season.
#'
#' @param season Season (YYYY) to pull streaks from. Defaults to `most_recent_pwhl_season()`.
#' @param game_type Game type: `"both"` (default), `"regular"`, `"playoffs"`,
#'   or `"preseason"`. When `"both"`, regular-season and playoff streaks are
#'   combined and labeled by a `game_type` column. Game types with no streak
#'   data for the season are omitted rather than erroring.
#' @return A data frame of class `fastRhockey_data` with player streak data
#'   including columns such as player_id, first_name, last_name, name, length,
#'   goals, assists, points, games_played, rank, team_name, team_code, and
#'   game_type. Returns NULL if unavailable.
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_streaks(season = 2025))
#' }

pwhl_streaks <- function(season = most_recent_pwhl_season(), game_type = "both") {

  game_type <- match.arg(
    game_type,
    choices = c("both", "regular", "playoffs", "preseason")
  )

  # "both" expands to regular + playoffs (preseason intentionally excluded).
  game_types <- if (game_type == "both") c("regular", "playoffs") else game_type

  streaks <- data.frame()

  for (gt in game_types) {
    df <- .pwhl_streaks_one(season = season, game_type = gt)
    if (!is.null(df) && nrow(df) > 0) {
      df$game_type <- gt
      streaks <- dplyr::bind_rows(streaks, df)
    }
  }

  if (nrow(streaks) == 0) {
    message(glue::glue("{Sys.time()}: No streak data found for season={season}."))
    return(NULL)
  }

  streaks <- make_fastRhockey_data(
    streaks,
    type = "PWHL Player Streaks",
    timestamp = Sys.time()
  )

  return(streaks)
}

#' Fetch PWHL player streaks for a single game type.
#'
#' Returns the cleaned streak data frame for one resolved season/game_type, or
#' NULL when the game type does not exist for the season or has no streaks.
#' Errors degrade to NULL so a combined ("both") call survives a game type
#' with no data.
#'
#' @param season Numeric season (concluding) year.
#' @param game_type Character: "regular", "playoffs", or "preseason".
#' @return A data frame, or NULL.
#' @keywords internal
#' @noRd
.pwhl_streaks_one <- function(season, game_type) {
  tryCatch(
    expr = {
      season_id <- .pwhl_resolve_season_id(season = season, game_type = game_type)

      url <- .pwhl_modulekit_url(list(
        view = "statviewtype",
        type = "streaks",
        season_id = season_id
      ))

      r <- .pwhl_api(url)

      players_raw <- r$SiteKit$Statviewtype

      streaks <- data.frame()

      if (!is.null(players_raw)) {
        for (i in seq_along(players_raw)) {
          p <- players_raw[[i]]
          if (!is.null(p)) {
            streak_row <- as.data.frame(
              lapply(p, function(x) if (is.null(x)) NA else as.character(x)),
              stringsAsFactors = FALSE
            )
            streaks <- dplyr::bind_rows(streaks, streak_row)
          }
        }
      }

      if (nrow(streaks) == 0) {
        return(NULL)
      }

      streaks <- streaks %>%
        janitor::clean_names()

      return(streaks)
    },
    error = function(e) {
      return(NULL)
    }
  )
}
