#' @title  **PWHL League Leaders**
#' @description Retrieves PWHL league leaders (top scorers or top goalies) for a season.
#'
#' @param position Either "skaters" (default) or "goalies".
#' @param season Season (YYYY) to pull leaders from. Defaults to `most_recent_pwhl_season()`.
#' @param game_type Game type: "regular" (default), "preseason", or "playoffs".
#' @param limit Maximum number of leaders to return. Default 100.
#' @return A data frame (`fastRhockey_data`) with the following columns, or NULL if unavailable:
#'
#'    |col_name                   |types     |description                                       |
#'    |:--------------------------|:---------|:-------------------------------------------------|
#'    |player_id                  |character |Unique player identifier.                         |
#'    |shortname                  |character |Player short name.                                |
#'    |first_name                 |character |Player first name.                                |
#'    |last_name                  |character |Player last name.                                 |
#'    |name                       |character |Player full name.                                 |
#'    |phonetic_name              |character |Phonetic spelling of the player name.             |
#'    |active                     |character |Whether the player is active.                     |
#'    |height                     |character |Player height.                                    |
#'    |weight                     |character |Player weight.                                    |
#'    |last_years_club            |character |Player's club in the previous season.             |
#'    |age                        |character |Player age.                                       |
#'    |shoots                     |character |Handedness the player shoots.                     |
#'    |position                   |character |Player position.                                  |
#'    |suspension_games_remaining |character |Suspension games remaining.                       |
#'    |suspension_indefinite      |character |Whether the suspension is indefinite.             |
#'    |rookie                     |character |Whether the player is a rookie.                   |
#'    |veteran                    |character |Whether the player is a veteran.                  |
#'    |draft_eligible             |character |Whether the player is draft eligible.             |
#'    |jersey_number              |character |Jersey number.                                    |
#'    |team_name                  |character |Team name.                                        |
#'    |team_code                  |character |Team abbreviation.                                |
#'    |team_id                    |character |Unique team identifier.                           |
#'    |division                   |character |Team division.                                    |
#'    |birthdate                  |character |Player birthdate.                                 |
#'    |birthdate_year             |character |Player birth year.                                |
#'    |hometown                   |character |Player hometown.                                  |
#'    |homeprov                   |character |Player home province/state.                       |
#'    |homecntry                  |character |Player home country.                              |
#'    |birthtown                  |character |Player birth town.                                |
#'    |birthprov                  |character |Player birth province/state.                      |
#'    |birthcntry                 |character |Player birth country.                             |
#'    |hometownprov               |character |Player hometown and province/state.               |
#'    |homeplace                  |character |Player home place description.                    |
#'    |games_played               |numeric   |Games played.                                     |
#'    |game_winning_goals         |character |Game-winning goals.                               |
#'    |game_tieing_goals          |character |Game-tying goals.                                 |
#'    |first_goals                |character |First goals of a game.                            |
#'    |insurance_goals            |character |Insurance goals.                                  |
#'    |unassisted_goals           |character |Unassisted goals.                                 |
#'    |empty_net_goals            |character |Empty-net goals.                                  |
#'    |overtime_goals             |character |Overtime goals.                                   |
#'    |ice_time                   |character |Total ice time.                                   |
#'    |ice_time_avg               |character |Average ice time.                                 |
#'    |goals                      |numeric   |Goals scored.                                     |
#'    |shots                      |numeric   |Shots on goal.                                    |
#'    |loose_ball_recoveries      |character |Loose ball recoveries.                            |
#'    |caused_turnovers           |character |Turnovers caused.                                 |
#'    |turnovers                  |character |Turnovers committed.                              |
#'    |hits                       |character |Hits delivered.                                   |
#'    |shots_blocked_by_player    |character |Shots blocked by the player.                      |
#'    |ice_time_minutes_seconds   |character |Ice time in minutes and seconds.                  |
#'    |shooting_percentage        |numeric   |Shooting percentage.                              |
#'    |assists                    |numeric   |Assists.                                          |
#'    |points                     |numeric   |Total points (goals + assists).                   |
#'    |points_per_game            |numeric   |Points per game.                                  |
#'    |plus_minus                 |numeric   |Plus/minus rating.                                |
#'    |penalty_minutes            |numeric   |Penalty minutes.                                  |
#'    |penalty_minutes_per_game   |character |Penalty minutes per game.                         |
#'    |ice_time_per_game_avg      |character |Average ice time per game.                        |
#'    |hits_per_game_avg          |character |Average hits per game.                            |
#'    |minor_penalties            |character |Minor penalties.                                  |
#'    |major_penalties            |character |Major penalties.                                  |
#'    |power_play_goals           |numeric   |Power-play goals.                                 |
#'    |power_play_assists         |numeric   |Power-play assists.                               |
#'    |power_play_points          |character |Power-play points.                                |
#'    |short_handed_goals         |numeric   |Short-handed goals.                               |
#'    |short_handed_assists       |numeric   |Short-handed assists.                             |
#'    |short_handed_points        |character |Short-handed points.                              |
#'    |shootout_goals             |character |Shootout goals.                                   |
#'    |shootout_attempts          |character |Shootout attempts.                                |
#'    |shootout_winning_goals     |character |Shootout game-winning goals.                      |
#'    |shootout_games_played      |character |Games played that went to a shootout.             |
#'    |faceoff_attempts           |character |Faceoff attempts.                                 |
#'    |faceoff_wins               |character |Faceoffs won.                                     |
#'    |faceoff_pct                |character |Faceoff win percentage.                           |
#'    |faceoff_wa                 |character |Faceoff wins-to-attempts metric.                  |
#'    |shots_on                   |character |Shots on goal count.                              |
#'    |shootout_percentage        |character |Shootout scoring percentage.                      |
#'    |latest_team_id             |character |Most recent team identifier.                      |
#'    |num_teams                  |character |Number of teams the player has played for.        |
#'    |logo                       |character |URL to the team logo.                             |
#'    |rank                       |numeric   |Leader rank.                                      |
#'    |player_page_link           |character |URL to the player page.                           |
#'    |namelink                   |character |HTML link for the player name.                    |
#'    |teamlink                   |character |HTML link for the team.                           |
#'    |photo                      |character |URL to the player photo.                          |
#'    |team_breakdown             |character |Per-team statistical breakdown.                   |
#'    |is_total                   |character |Whether the row is a season total.                |
#' @import jsonlite
#' @import dplyr
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
