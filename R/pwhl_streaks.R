#' @title  **PWHL Player Streaks**
#' @description Retrieves player streak data for a PWHL season.
#'
#' @param season Season (YYYY) to pull streaks from. Defaults to `most_recent_pwhl_season()`.
#' @param game_type Game type: "regular" (default), "preseason", or "playoffs".
#' @return A data frame of class `fastRhockey_data` with player streak data
#'   including columns such as player_id, first_name, last_name, name, length,
#'   goals, assists, points, games_played, rank, team_name, team_code.
#'   Returns NULL if unavailable.
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_streaks(season = 2025))
#' }

pwhl_streaks <- function(season = most_recent_pwhl_season(), game_type = "regular") {

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
        message(glue::glue("{Sys.time()}: No streak data found for season={season}."))
        return(NULL)
      }

      streaks <- streaks %>%
        janitor::clean_names()

      streaks <- make_fastRhockey_data(
        streaks,
        type = "PWHL Player Streaks",
        timestamp = Sys.time()
      )

      return(streaks)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Error retrieving PWHL streaks. {e$message}"))
      return(NULL)
    }
  )
}
