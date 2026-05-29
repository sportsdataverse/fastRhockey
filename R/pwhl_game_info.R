#' @title  **PWHL Game Information**
#' @description PWHL Game Information
#'
#' @param game_id Game ID that you want game information for
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name        |types     |description                                  |
#'    |:---------------|:---------|:--------------------------------------------|
#'    |game_id         |integer   |Unique game identifier.                      |
#'    |game_season     |numeric   |Season (concluding year, YYYY).              |
#'    |game_type       |character |Game type the row belongs to.                |
#'    |game_date       |character |Game date.                                   |
#'    |home_team       |character |Home team name.                              |
#'    |away_team       |character |Away team name.                              |
#'    |home_team_id    |integer   |Home team identifier.                        |
#'    |away_team_id    |integer   |Away team identifier.                        |
#'    |home_score      |integer   |Home team final score.                       |
#'    |away_score      |integer   |Away team final score.                       |
#'    |game_duration   |character |Game duration.                               |
#'    |game_venue      |character |Venue where the game was played.             |
#'    |game_report     |character |URL to the game report.                      |
#'    |game_boxscore   |character |URL to the text box score.                   |
#'    |game_season_id  |character |Season identifier used by the PWHL feed.     |
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
    "game_season" = c(seasons %>% dplyr::filter(.data$season_id == r$details$seasonId) %>% dplyr::pull("season_yr")),
    "game_type" = c(seasons %>% dplyr::filter(.data$season_id == r$details$seasonId) %>% dplyr::pull("game_type_label")),
    "game_date" = c(r$details$date),
    "home_team" = c(r$homeTeam$info$name),
    "away_team" = c(r$visitingTeam$info$name),
    "home_team_id" = c(r$homeTeam$info$id),
    "away_team_id" = c(r$visitingTeam$info$id),
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
