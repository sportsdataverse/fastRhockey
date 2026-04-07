#' @title **NHL Player Stats**
#' @description Returns season-by-season stats for a given player ID.
#' Uses the new NHL API player landing endpoint (`api-web.nhle.com`).
#'
#' **Note:** The original `statsapi.web.nhl.com` endpoint has been retired.
#' This function now returns the `seasonTotals` data from the player landing
#' page, which includes year-by-year stats across all leagues (NHL, AHL, junior, etc.).
#'
#' @param player_id Player unique ID (e.g., 8476899)
#' @return Returns a data frame with season-by-season stats for the player,
#'   including games played, goals, assists, and league-specific columns.
#' @keywords NHL Player Stats
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr tibble bind_cols
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   try(nhl_player_stats(player_id = 8476899))
#' }
nhl_player_stats <- function(player_id) {
  url <- glue::glue("https://api-web.nhle.com/v1/player/{player_id}/landing")

  tryCatch(
    expr = {
      res <- httr::RETRY("GET", url)
      check_status(res)

      resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
      raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

      # seasonTotals contains year-by-year stats across all leagues
      season_totals <- raw[["seasonTotals"]]

      if (is.null(season_totals) || length(season_totals) == 0) {
        message(glue::glue("{Sys.time()}: No season stats found for player {player_id}"))
        return(NULL)
      }

      # Flatten the season totals — jsonlite may leave nested list-columns
      # for localized name fields (teamName, teamCommonName, etc.)
      stats_df <- jsonlite::fromJSON(
        jsonlite::toJSON(season_totals, auto_unbox = TRUE),
        flatten = TRUE
      )

      # Add player biographical context
      stats_df$player_id <- raw$playerId
      stats_df$first_name <- raw$firstName$default
      stats_df$last_name <- raw$lastName$default
      stats_df$position <- raw$position

      stats_df <- stats_df %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Player Stats Information from NHL.com", Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no player stats data for {player_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(stats_df)
}
