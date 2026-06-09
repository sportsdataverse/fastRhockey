#' @title  **PWHL Scorebar**
#' @description Retrieves recent and upcoming PWHL game scores.
#'
#' @param days_back Number of days back to include. Default 3.
#' @param days_ahead Number of days ahead to include. Default 3.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name       |types     |description                            |
#'    |:--------------|:---------|:--------------------------------------|
#'    |game_id        |numeric   |Unique game identifier.                |
#'    |season_id      |numeric   |Season identifier.                     |
#'    |date           |character |Game date.                             |
#'    |game_date      |character |Game date.                             |
#'    |status         |character |Status of the game.                    |
#'    |home_team      |character |Home team name.                        |
#'    |home_team_id   |numeric   |Home team identifier.                  |
#'    |home_team_code |character |Home team abbreviation.                |
#'    |home_score     |character |Home team score.                       |
#'    |away_team      |character |Away team name.                        |
#'    |away_team_id   |numeric   |Away team identifier.                  |
#'    |away_team_code |character |Away team abbreviation.                |
#'    |away_score     |character |Away team score.                       |
#'    |period         |character |Current period for live/completed games.|
#'    |clock          |character |Current clock time for live games.     |
#' @import jsonlite
#' @import dplyr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_scorebar(days_back = 7, days_ahead = 7))
#' }

pwhl_scorebar <- function(days_back = 3, days_ahead = 3) {

  tryCatch(
    expr = {
      url <- .pwhl_modulekit_url(list(
        view = "scorebar",
        numberofdaysback = as.character(days_back),
        numberofdaysahead = as.character(days_ahead)
      ))

      r <- .pwhl_api(url)

      games_raw <- r$SiteKit$Scorebar

      games <- data.frame()

      for (i in seq_along(games_raw)) {
        g <- games_raw[[i]]
        game_row <- data.frame(
          game_id = as.numeric(g$ID %||% NA),
          season_id = as.numeric(g$SeasonID %||% NA),
          date = as.character(g$Date %||% NA),
          game_date = as.character(g$GameDate %||% NA),
          status = as.character(g$GameStatus %||% NA),
          home_team = as.character(g$HomeLongName %||% NA),
          home_team_id = as.numeric(g$HomeID %||% NA),
          home_team_code = as.character(g$HomeCode %||% NA),
          home_score = as.character(g$HomeGoals %||% ""),
          away_team = as.character(g$VisitorLongName %||% NA),
          away_team_id = as.numeric(g$VisitorID %||% NA),
          away_team_code = as.character(g$VisitorCode %||% NA),
          away_score = as.character(g$VisitorGoals %||% ""),
          period = as.character(g$Period %||% NA),
          clock = as.character(g$GameClock %||% NA),
          stringsAsFactors = FALSE
        )
        games <- dplyr::bind_rows(games, game_row)
      }

      if (nrow(games) == 0) {
        message(glue::glue("{Sys.time()}: No scorebar data found."))
        return(NULL)
      }

      games <- make_fastRhockey_data(
        games,
        type = "PWHL Scorebar",
        timestamp = Sys.time()
      )

      return(games)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Error retrieving PWHL scorebar. {e$message}"))
      return(NULL)
    }
  )
}
