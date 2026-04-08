#' @title  **PWHL League Leaders**
#' @description Retrieves PWHL league leaders (top scorers or top goalies) for a season.
#'
#' @param position Either "skaters" (default) or "goalies".
#' @param season Season (YYYY) to pull leaders from. Defaults to `most_recent_pwhl_season()`.
#' @param game_type Game type: "regular" (default), "preseason", or "playoffs".
#' @param limit Maximum number of leaders to return. Default 100.
#' @return A data frame with league leader statistics, or NULL if unavailable.
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_leaders(position = "skaters", season = 2025))
#'   try(pwhl_leaders(position = "goalies", season = 2025))
#' }

pwhl_leaders <- function(position = "skaters", season = most_recent_pwhl_season(),
                         game_type = "regular", limit = 100) {

  tryCatch(
    expr = {
      season_id <- .pwhl_resolve_season_id(season = season, game_type = game_type)

      type_param <- if (position == "goalies") "topgoalies" else "topscorers"

      url <- .pwhl_modulekit_url(list(
        view = "statviewtype",
        type = type_param,
        first = "0",
        limit = as.character(limit),
        season_id = season_id
      ))

      r <- .pwhl_api(url)

      players_raw <- r$SiteKit$Statviewtype

      leaders <- data.frame()

      if (!is.null(players_raw)) {
        for (i in seq_along(players_raw)) {
          p <- players_raw[[i]]
          if (!is.null(p)) {
            leader_row <- as.data.frame(
              lapply(p, function(x) if (is.null(x)) NA else as.character(x)),
              stringsAsFactors = FALSE
            )
            leaders <- dplyr::bind_rows(leaders, leader_row)
          }
        }
      }

      if (nrow(leaders) == 0) {
        message(glue::glue("{Sys.time()}: No leader data found for position={position}, season={season}."))
        return(NULL)
      }

      leaders <- leaders %>%
        janitor::clean_names()

      # Convert numeric columns
      num_cols <- intersect(
        names(leaders),
        c("rank", "games_played", "goals", "assists", "points", "plus_minus",
          "penalty_minutes", "shots", "shooting_percentage", "points_per_game",
          "power_play_goals", "power_play_assists", "short_handed_goals",
          "short_handed_assists", "wins", "losses", "goals_against_average",
          "save_percentage", "shutouts", "saves")
      )
      for (col in num_cols) {
        leaders[[col]] <- suppressWarnings(as.numeric(leaders[[col]]))
      }

      leaders <- make_fastRhockey_data(
        leaders,
        type = glue::glue("PWHL Leaders - {position}"),
        timestamp = Sys.time()
      )

      return(leaders)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Error retrieving PWHL leaders. {e$message}"))
      return(NULL)
    }
  )
}
