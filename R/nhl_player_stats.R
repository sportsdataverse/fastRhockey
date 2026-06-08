#' @title **NHL Player Stats**
#' @description Returns season-by-season stats for a given player ID.
#' Uses the new NHL API player landing endpoint (`api-web.nhle.com`).
#'
#' **Note:** The original `statsapi.web.nhl.com` endpoint has been retired.
#' This function now returns the `seasonTotals` data from the player landing
#' page, which includes year-by-year stats across all leagues (NHL, AHL, junior, etc.).
#'
#' @param player_id Player unique ID (e.g., 8476899)
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                                 |types     |description                                       |
#'    |:----------------------------------------|:---------|:-------------------------------------------------|
#'    |game_type_id                             |integer   |Game type identifier (2 regular, 3 playoffs).     |
#'    |games_played                             |integer   |Games played in the season.                       |
#'    |goals_against_avg                        |numeric   |Goalie goals-against average.                     |
#'    |league_abbrev                            |character |League abbreviation.                              |
#'    |save_pctg                                |numeric   |Goalie save percentage.                           |
#'    |season                                   |integer   |Season (concluding year, YYYY).                   |
#'    |sequence                                 |integer   |Sequence order of the season row.                 |
#'    |team_name_default                        |character |Team name (default localization).                 |
#'    |goals_against                            |integer   |Goals against.                                    |
#'    |losses                                   |integer   |Losses.                                           |
#'    |shutouts                                 |integer   |Shutouts.                                         |
#'    |ties                                     |integer   |Ties.                                             |
#'    |time_on_ice                              |character |Time on ice.                                      |
#'    |wins                                     |integer   |Wins.                                             |
#'    |shots_against                            |integer   |Shots faced.                                      |
#'    |team_name_cs                             |character |Team name (Czech localization).                   |
#'    |team_name_de                             |character |Team name (German localization).                  |
#'    |team_name_es                             |character |Team name (Spanish localization).                 |
#'    |team_name_fi                             |character |Team name (Finnish localization).                 |
#'    |team_name_sk                             |character |Team name (Slovak localization).                  |
#'    |team_name_sv                             |character |Team name (Swedish localization).                 |
#'    |team_common_name_default                 |character |Team common name (default localization).          |
#'    |team_common_name_cs                      |character |Team common name (Czech localization).            |
#'    |team_common_name_de                      |character |Team common name (German localization).           |
#'    |team_common_name_es                      |character |Team common name (Spanish localization).          |
#'    |team_common_name_fi                      |character |Team common name (Finnish localization).          |
#'    |team_common_name_sk                      |character |Team common name (Slovak localization).           |
#'    |team_common_name_sv                      |character |Team common name (Swedish localization).          |
#'    |team_place_name_with_preposition_default |character |Team place name with preposition (default).       |
#'    |ot_losses                                |integer   |Overtime losses.                                  |
#'    |assists                                  |integer   |Assists.                                          |
#'    |games_started                            |integer   |Games started (goalies).                          |
#'    |goals                                    |integer   |Goals scored.                                     |
#'    |pim                                      |integer   |Penalty minutes.                                  |
#'    |team_name_fr                             |character |Team name (French localization).                  |
#'    |team_place_name_with_preposition_fr      |character |Team place name with preposition (French).        |
#'    |team_common_name_fr                      |character |Team common name (French localization).           |
#'    |player_id                                |integer   |Unique player identifier.                         |
#'    |first_name                               |character |Player first name.                                |
#'    |last_name                                |character |Player last name.                                 |
#'    |position                                 |character |Player position.                                  |
#' @keywords NHL Player Stats
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
      res <- .retry_request(url)
      check_status(res)

      resp_text <- .resp_text(res)
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
