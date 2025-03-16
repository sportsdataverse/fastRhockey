#' @title  **PWHL Game Information**
#' @description PWHL Game Information
#'
#' @param game_id Game ID that you want game information for
#' @return A data frame with game id / date, game metadata etc from the PWHL
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples \donttest{
#'   try(pwhl_game_info(game_id = 27))
#' }

pwhl_game_info <- function(game_id) {
  URL <- glue::glue("https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=gameSummary&game_id={game_id}&key=446521baf8c38984&site_id=0&client_code=pwhl&lang=en&league_id=&callback=angular.callbacks._5")

  res <- httr::RETRY("GET", URL)
  res <- res %>%
    httr::content(as = "text", encoding = "utf-8")
  callback_pattern <- "angular.callbacks._\\d+\\("
  res <- gsub(callback_pattern, "", res)
  res <- gsub("}}})", "}}}", res)

  r <- res %>% jsonlite::parse_json()

  seasons <- pwhl_season_id()

  game_details <- data.frame(
    "game_id" = c(r$details$id),
    "game_season" = c(seasons %>% dplyr::filter(season_id == r$details$seasonId) %>% pull(season_yr)),
    "game_type" = c(seasons %>% dplyr::filter(season_id == r$details$seasonId) %>% pull(game_type_label)),
    "game_date" = c(r$details$date),
    "home_team" = c(r$homeTeam$info$name),
    "away_team" = c(r$visitingTeam$info$name),
    "home_score" = c(r$homeTeam$stats$goals),
    "away_score" = c(r$visitingTeam$stats$goals),
    "game_duration" = c(r$details$duration),
    "game_venue" = c(r$details$venue),
    "game_report" = c(r$details$gameReportUrl),
    "game_boxscore" = c(r$details$textBoxscoreUrl),
    "game_season_id" = c(r$details$seasonId)
  )

  return(game_details)

}
