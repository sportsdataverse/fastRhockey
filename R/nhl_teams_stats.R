#' @title **NHL Teams Stats**
#' @description Returns NHL team player-level stats (skaters and goalies) for a
#' given team abbreviation and season.
#' Uses the new NHL API club-stats endpoint (`api-web.nhle.com`).
#'
#' **Breaking change:** The old `team_id` (integer) parameter has been replaced
#' by `team_abbr` (3-letter string). The `season` parameter now accepts a
#' 4-digit year (e.g., 2024 for the 2024-25 season).
#'
#' @param team_abbr Three-letter team abbreviation (e.g., "TBL", "TOR", "SEA")
#' @param season Integer 4-digit year (e.g., 2024 for the 2024-25 season).
#'   If NULL, returns current season stats.
#' @param game_type Integer game type: 2 = regular season (default), 3 = playoffs
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                 |types     |description                                          |
#'    |:------------------------|:---------|:----------------------------------------------------|
#'    |player_id                |integer   |Unique player identifier.                            |
#'    |headshot                 |character |URL to the player's headshot image.                  |
#'    |position_code            |character |Player position code.                                |
#'    |games_played             |integer   |Games played.                                        |
#'    |goals                    |integer   |Goals scored.                                        |
#'    |assists                  |integer   |Assists.                                             |
#'    |points                   |integer   |Total points (goals + assists).                      |
#'    |plus_minus               |integer   |Plus/minus rating.                                   |
#'    |penalty_minutes          |integer   |Penalty minutes.                                     |
#'    |power_play_goals         |integer   |Power-play goals.                                    |
#'    |shorthanded_goals        |integer   |Short-handed goals.                                  |
#'    |game_winning_goals       |integer   |Game-winning goals.                                  |
#'    |overtime_goals           |integer   |Overtime goals.                                      |
#'    |shots                    |integer   |Shots on goal.                                       |
#'    |shooting_pctg            |numeric   |Shooting percentage.                                 |
#'    |avg_time_on_ice_per_game |numeric   |Average time on ice per game.                        |
#'    |avg_shifts_per_game      |numeric   |Average shifts per game.                             |
#'    |faceoff_win_pctg         |numeric   |Faceoff win percentage.                              |
#'    |first_name_default       |character |Player first name (default language).                |
#'    |last_name_default        |character |Player last name (default language).                 |
#'    |last_name_cs             |character |Player last name (Czech localization).               |
#'    |last_name_fi             |character |Player last name (Finnish localization).             |
#'    |last_name_sk             |character |Player last name (Slovak localization).              |
#'    |player_type              |character |Player type ("skater" or "goalie").                  |
#'    |games_started            |integer   |Games started (goalies).                             |
#'    |wins                     |integer   |Wins (goalies).                                      |
#'    |losses                   |integer   |Losses (goalies).                                    |
#'    |overtime_losses          |integer   |Overtime losses (goalies).                           |
#'    |goals_against_average    |numeric   |Goals-against average (goalies).                     |
#'    |save_percentage          |numeric   |Save percentage (goalies).                           |
#'    |shots_against            |integer   |Shots faced (goalies).                               |
#'    |saves                    |integer   |Saves made (goalies).                                |
#'    |goals_against            |integer   |Goals against (goalies).                             |
#'    |shutouts                 |integer   |Shutouts (goalies).                                  |
#'    |time_on_ice              |integer   |Total time on ice (goalies).                         |
#'    |first_name_cs            |character |Player first name (Czech localization).              |
#'    |first_name_sk            |character |Player first name (Slovak localization).             |
#'    |team_abbr                |character |Team abbreviation.                                   |
#'    |season                   |character |Season identifier.                                   |
#'    |game_type                |integer   |Game type (2 = regular season, 3 = playoffs).        |
#' @keywords NHL Teams Stats
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr bind_rows mutate
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   try(nhl_teams_stats(team_abbr = "TBL"))
#' }
nhl_teams_stats <- function(team_abbr, season = NULL, game_type = 2) {
  if (is.null(season)) {
    url <- glue::glue(
      "https://api-web.nhle.com/v1/club-stats/{team_abbr}/now"
    )
  } else {
    api_season <- paste0(season, season + 1)
    url <- glue::glue(
      "https://api-web.nhle.com/v1/club-stats/{team_abbr}/{api_season}/{game_type}"
    )
  }

  tryCatch(
    expr = {
      res <- httr::RETRY("GET", url)
      check_status(res)

      resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
      raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

      result_frames <- list()

      # Parse skaters
      if (!is.null(raw[["skaters"]]) && length(raw[["skaters"]]) > 0) {
        skaters <- jsonlite::fromJSON(
          jsonlite::toJSON(raw[["skaters"]], auto_unbox = TRUE),
          flatten = TRUE
        )
        skaters$player_type <- "skater"
        result_frames[["skaters"]] <- skaters
      }

      # Parse goalies
      if (!is.null(raw[["goalies"]]) && length(raw[["goalies"]]) > 0) {
        goalies <- jsonlite::fromJSON(
          jsonlite::toJSON(raw[["goalies"]], auto_unbox = TRUE),
          flatten = TRUE
        )
        goalies$player_type <- "goalie"
        result_frames[["goalies"]] <- goalies
      }

      if (length(result_frames) == 0) {
        message(glue::glue(
          "{Sys.time()}: No stats found for {team_abbr}"
        ))
        return(NULL)
      }

      stats_df <- dplyr::bind_rows(result_frames)
      stats_df$team_abbr <- team_abbr
      stats_df$season <- .safe_val(raw$season)
      stats_df$game_type <- .safe_val(raw$gameType)

      stats_df <- stats_df %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Teams Stats Information from NHL.com", Sys.time())
    },
    error = function(e) {
      message(glue::glue(
        "{Sys.time()}: Invalid arguments or no team stats data for {team_abbr} available!"
      ))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(stats_df)
}

# Internal helper — safely extract a value or return NA
.safe_val <- function(x, default = NA) {
  if (is.null(x)) default else x
}
