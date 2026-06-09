#' @title  **PWHL Play-by-play**
#' @description PWHL Play-by-play
#'
#' @param game_id Game ID that you want play-by-play for
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                   |types     |description                                                        |
#'    |:--------------------------|:---------|:------------------------------------------------------------------|
#'    |game_id                    |numeric   |Unique game identifier.                                            |
#'    |event                      |character |Event type (faceoff, shot, goal, penalty, blocked_shot, hit, etc.).|
#'    |team_id                    |numeric   |Unique team identifier for the event.                              |
#'    |period_of_game             |character |Period number within the game.                                     |
#'    |time_of_period             |character |Elapsed time within the period (MM:SS).                            |
#'    |player_id                  |integer   |Primary player's unique identifier.                                |
#'    |player_name_first          |character |Primary player first name.                                         |
#'    |player_name_last           |character |Primary player last name.                                          |
#'    |player_position            |character |Primary player position.                                           |
#'    |player_two_id              |integer   |Second player's unique identifier.                                 |
#'    |player_two_name_first      |character |Second player first name.                                          |
#'    |player_two_name_last       |character |Second player last name.                                           |
#'    |player_two_position        |character |Second player position.                                            |
#'    |x_coord                    |numeric   |Transformed x-coordinate of the event (feet scale).               |
#'    |y_coord                    |numeric   |Transformed y-coordinate of the event (feet scale).               |
#'    |home_win                   |numeric   |Whether the home player won the faceoff.                           |
#'    |player_team_id             |numeric   |Unique team identifier of the primary player.                      |
#'    |event_type                 |character |Detailed event sub-type (shot type, penalty type).                 |
#'    |shot_quality               |character |Shot quality rating.                                               |
#'    |goal                       |logical   |Whether the shot resulted in a goal.                               |
#'    |goalie_id                  |integer   |Goalie's unique identifier.                                        |
#'    |goalie_first               |character |Goalie first name.                                                 |
#'    |goalie_last                |character |Goalie last name.                                                  |
#'    |penalty_length             |character |Penalty length in minutes.                                         |
#'    |power_play                 |numeric   |Whether the event occurred on a power play.                        |
#'    |player_three_id            |integer   |Third player's unique identifier.                                  |
#'    |player_three_name_first    |character |Third player first name.                                           |
#'    |player_three_name_last     |character |Third player last name.                                            |
#'    |player_three_position      |character |Third player position.                                             |
#'    |empty_net                  |character |Whether the goal was scored on an empty net.                       |
#'    |game_winner                |character |Whether the goal was the game-winning goal.                        |
#'    |penalty_shot               |character |Whether the goal came on a penalty shot.                           |
#'    |insurance                  |character |Whether the goal was an insurance goal.                            |
#'    |short_handed               |character |Whether the event occurred while short-handed.                     |
#'    |plus_player_one_id         |integer   |On-ice plus player one unique identifier.                          |
#'    |plus_player_one_first      |character |On-ice plus player one first name.                                 |
#'    |plus_player_one_last       |character |On-ice plus player one last name.                                  |
#'    |plus_player_one_position   |character |On-ice plus player one position.                                   |
#'    |plus_player_two_id         |integer   |On-ice plus player two unique identifier.                          |
#'    |plus_player_two_first      |character |On-ice plus player two first name.                                 |
#'    |plus_player_two_last       |character |On-ice plus player two last name.                                  |
#'    |plus_player_two_position   |character |On-ice plus player two position.                                   |
#'    |plus_player_three_id       |integer   |On-ice plus player three unique identifier.                        |
#'    |plus_player_three_first    |character |On-ice plus player three first name.                               |
#'    |plus_player_three_last     |character |On-ice plus player three last name.                                |
#'    |plus_player_three_position |character |On-ice plus player three position.                                 |
#'    |plus_player_four_id        |integer   |On-ice plus player four unique identifier.                         |
#'    |plus_player_four_first     |character |On-ice plus player four first name.                                |
#'    |plus_player_four_last      |character |On-ice plus player four last name.                                 |
#'    |plus_player_four_position  |character |On-ice plus player four position.                                  |
#'    |plus_player_five_id        |integer   |On-ice plus player five unique identifier.                         |
#'    |plus_player_five_first     |character |On-ice plus player five first name.                                |
#'    |plus_player_five_last      |character |On-ice plus player five last name.                                 |
#'    |plus_player_five_position  |character |On-ice plus player five position.                                  |
#'    |minus_player_one_id        |integer   |On-ice minus player one unique identifier.                         |
#'    |minus_player_one_first     |character |On-ice minus player one first name.                                |
#'    |minus_player_one_last      |character |On-ice minus player one last name.                                 |
#'    |minus_player_one_position  |character |On-ice minus player one position.                                  |
#'    |minus_player_two_id        |integer   |On-ice minus player two unique identifier.                         |
#'    |minus_player_two_first     |character |On-ice minus player two first name.                                |
#'    |minus_player_two_last      |character |On-ice minus player two last name.                                 |
#'    |minus_player_two_position  |character |On-ice minus player two position.                                  |
#'    |minus_player_three_id      |integer   |On-ice minus player three unique identifier.                       |
#'    |minus_player_three_first   |character |On-ice minus player three first name.                              |
#'    |minus_player_three_last    |character |On-ice minus player three last name.                               |
#'    |minus_player_three_position|character |On-ice minus player three position.                                |
#'    |minus_player_four_id       |integer   |On-ice minus player four unique identifier.                        |
#'    |minus_player_four_first    |character |On-ice minus player four first name.                               |
#'    |minus_player_four_last     |character |On-ice minus player four last name.                                |
#'    |minus_player_four_position |character |On-ice minus player four position.                                 |
#'    |minus_player_five_id       |integer   |On-ice minus player five unique identifier.                        |
#'    |minus_player_five_first    |character |On-ice minus player five first name.                               |
#'    |minus_player_five_last     |character |On-ice minus player five last name.                                |
#'    |minus_player_five_position |character |On-ice minus player five position.                                 |
#'    |game_date                  |character |Game date.                                                         |
#'    |game_season                |numeric   |Season (concluding year, YYYY).                                    |
#'    |game_season_id             |character |Season identifier.                                                 |
#'    |home_team_id               |integer   |Home team unique identifier.                                       |
#'    |home_team                  |character |Home team name.                                                    |
#'    |away_team_id               |integer   |Away team unique identifier.                                       |
#'    |away_team                  |character |Away team name.                                                    |
#'    |x_coord_original           |integer   |Original raw x-coordinate from the feed.                          |
#'    |y_coord_original           |integer   |Original raw y-coordinate from the feed.                          |
#'    |x_coord_neutral            |numeric   |Neutral-zone-centered x-coordinate.                               |
#'    |y_coord_neutral            |numeric   |Neutral-zone-centered y-coordinate.                               |
#'    |x_coord_fixed              |numeric   |Fixed-projection x-coordinate.                                    |
#'    |y_coord_fixed              |numeric   |Fixed-projection y-coordinate.                                    |
#'    |x_coord_right              |numeric   |Right-oriented x-coordinate.                                      |
#'    |y_coord_right              |numeric   |Right-oriented y-coordinate.                                      |
#'    |x_coord_vertical           |numeric   |Vertical-projection x-coordinate.                                 |
#'    |y_coord_vertical           |numeric   |Vertical-projection y-coordinate.                                 |
#'    |minute_start               |numeric   |Minute mark of the period when the event started.                 |
#'    |second_start               |numeric   |Second mark of the period when the event started.                 |
#'    |clock                      |character |Game clock time remaining (MM:SS).                                |
#'    |sec_from_start             |numeric   |Seconds elapsed since the start of the game.                      |
#'    |shot_distance              |numeric   |Distance from the net in feet (shot/blocked_shot/goal events).    |
#'    |shot_angle                 |numeric   |Angle from the net in degrees (shot/blocked_shot/goal events).    |
#'    |scoring_chance             |logical   |TRUE when event is a shot-type within 25 ft of the net.           |
#'    |on_ice_home                |character |Comma-joined sorted player_ids on ice for the home team.          |
#'    |on_ice_away                |character |Comma-joined sorted player_ids on ice for the away team.          |
#' @import jsonlite
#' @import dplyr
#' @importFrom glue glue
#' @export
#' @examples \donttest{
#' try(pwhl_pbp(game_id = 27))
#' }
pwhl_pbp <- function(game_id) {
  df <- data.frame()

  tryCatch(
    expr = {
      # Fetch the gameCenterPlayByPlay payload via the shared HockeyTech URL
      # builder and API helper (handles JSONP stripping).
      pbp_url <- .hockeytech_url(
        "pwhl",
        "statviewfeed",
        "gameCenterPlayByPlay",
        list(game_id = game_id, league_id = "")
      )
      payload <- .hockeytech_api(pbp_url)

      # Parse the raw event list into the full column superset.
      df <- .parse_hockeytech_pbp(payload, pbp_style = "hockeytech_a",
                                   game_id = game_id)

      # Enrich: coordinate transforms, clock columns, PP/SH back-fill,
      # shot geometry, and on-ice player tracking (live meta + shifts fetch).
      df <- hockeytech_enrich_pbp(df, "pwhl", game_id)

      # Stamp with fastRhockey metadata class.
      df <- make_fastRhockey_data(df, "PWHL Play-by-Play data from HockeyTech",
                                   Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Error encountered: {e$message}. Please verify the game_id and ensure it corresponds to a valid game in the PWHL.")
    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(df)
}
