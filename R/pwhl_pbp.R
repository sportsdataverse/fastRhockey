#' @title  **PWHL Play-by-play**
#' @description PWHL Play-by-play
#'
#' @param game_id Game ID that you want play-by-play for
#' @return A data frame with play-by-play data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_team_roster(season = 2023, team = "Toronto"))
#' }


pwhl_pbp <- function(game_id) {

  URL <- paste0("https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=gameCenterPlayByPlay&game_id=", game_id, "&key=694cfeed58c932ee&client_code=pwhl&lang=en&league_id=&callback=angular.callbacks._8")

  res <- httr::RETRY("GET", URL)
  res <- res %>%
    httr::content(as = "text", encoding = "utf-8")
  res <- gsub("angular.callbacks._8\\(", "", res)
  res <- gsub("}}])", "}}]", res)

  r <- res %>% jsonlite::parse_json()

  events <- c("goalie_change", "faceoff", "shot", "penalty", "goal")

  goalie_events <- data.frame()
  game_events <- data.frame()

  tryCatch(
    expr = {
      for (y in 1:length(r)) {

        event <- coalesce(r[[y]]$event, NA)
        team_id <- as.numeric(coalesce(r[[y]]$details$team_id, NA))
        period_of_game <- coalesce(r[[y]]$details$period$id, NA)
        time_of_period <- coalesce(r[[y]]$details$time, NA)

        if (event == "goalie_change") {
          player_out <- coalesce(r[[y]]$details$goalieGoingOut, NA)
          player_in <- coalesce(r[[y]]$details$goalieComingIn, NA)

          if (! is.null(player_in)) {

            player_in_id <- coalesce(r[[y]]$details$goalieComingIn$id, NA)
            player_in_first <- coalesce(r[[y]]$details$goalieComingIn$firstName, NA)
            player_in_last <- coalesce(r[[y]]$details$goalieComingIn$lastName, NA)

            goalie <- data.frame(
              substitution = c("in"),
              goalie_id = c(player_in_id),
              goalie_name_first = c(player_in_first),
              goalie_name_last = c(player_in_last)
            )

          } else {
            player_in_id <- coalesce(NULL, NA)
            player_in_first <- coalesce(NULL, NA)
            player_in_last <- coalesce(NULL, NA)
          }

          if (! is.null(player_out)) {

            player_out_id <- coalesce(r[[y]]$details$goalieGoingOut$id, NA)
            player_out_first <- coalesce(r[[y]]$details$goalieGoingOut$firstName, NA)
            player_out_last <- coalesce(r[[y]]$details$goalieGoingOut$lastName, NA)

            goalie <- data.frame(
              substitution = c("in"),
              goalie_id = c(player_in_id),
              goalie_name_first = c(player_in_first),
              goalie_name_last = c(player_in_last)
            )

          } else {
            player_out_id <- NA
            player_out_first <- NA
            player_out_last <- NA
          }

          goalie$event <- event
          goalie$team_id <- team_id
          goalie$period_of_game <- period_of_game
          goalie$time_of_period <- time_of_period

          goalie_events <- bind_rows(goalie_events, goalie)

        } else if (event == "shot") {
            shooter_id <- coalesce(r[[y]]$details$shooter$id, NA)
            shooter_first <- coalesce(r[[y]]$details$shooter$firstName, NA)
            shooter_last <- coalesce(r[[y]]$details$shooter$lastName, NA)
            shooter_jersey <- coalesce(r[[y]]$details$shooter$jerseyNumber, NA)
            shooter_position <- coalesce(r[[y]]$details$shooter$position, NA)

            goal <- coalesce(r[[y]]$details$isGoal, NA)

            goalie_id <- coalesce(r[[y]]$details$goalie$id, NA)
            goalie_first <- coalesce(r[[y]]$details$goalie$firstName, NA)
            goalie_last <- coalesce(r[[y]]$details$goalie$lastName, NA)

            team_id <- as.numeric(coalesce(r[[y]]$details$shooterTeamId), NA)
            shot_quality <- coalesce(r[[y]]$details$shotQuality, NA)
            shot_type <- coalesce(r[[y]]$details$shotType, NA)
            x_loc <- coalesce(r[[y]]$details$xLocation, NA)
            y_loc <- coalesce(r[[y]]$details$yLocation, NA)

            shot_info <- data.frame(
              event = c(event),
              team_id = c(team_id),
              period_of_game = c(period_of_game),
              time_of_period = c(time_of_period),
              player_id = c(shooter_id),
              player_name_first = c(shooter_first),
              player_name_last = c(shooter_last),
              player_position = c(shooter_position),
              player_team_id = c(team_id),
              event_type = c(shot_type),
              shot_quality = c(shot_quality),
              goal = c(goal),
              x_coord = c(x_loc),
              y_coord = c(y_loc),
              goalie_id = c(goalie_id),
              goalie_first = c(goalie_first),
              goalie_last = c(goalie_last)
            )

            game_events <- bind_rows(game_events, shot_info)

          } else if (event == "goal") {
            scorer_id <- coalesce(r[[y]]$details$scoredBy$id, NA)
            scorer_first <- coalesce(r[[y]]$details$scoredBy$firstName, NA)
            scorer_last <- coalesce(r[[y]]$details$scoredBy$lastName, NA)
            scorer_pos <- coalesce(r[[y]]$details$scoredBy$position, NA)
            team_id <- as.numeric(coalesce(r[[y]]$details$team$id), NA)

            primary_assist_id <- coalesce(r[[y]]$details$assists[[1]]$id, NA)
            primary_assist_first <- coalesce(r[[y]]$details$assists[[1]]$firstName, NA)
            primary_assist_last <- coalesce(r[[y]]$details$assists[[1]]$lastName, NA)
            primary_assist_pos <- coalesce(r[[y]]$details$assists[[1]]$position, NA)

            if (as.numeric(r[[y]]$details$assistNumbers[[2]]) > 0) {

              sec_assist_id <- coalesce(r[[y]]$details$assists[[2]]$id, NA)
              sec_assist_first <- coalesce(r[[y]]$details$assists[[2]]$firstName, NA)
              sec_assist_last <- coalesce(r[[y]]$details$assists[[2]]$lastName, NA)
              sec_assist_pos <- coalesce(r[[y]]$details$assists[[2]]$position, NA)

            } else {

              sec_assist_id <- NA
              sec_assist_first <- NA
              sec_assist_last <- NA
              sec_assist_pos <- NA

            }

            empty_net <- coalesce(r[[y]]$details$properties$isEmptyNet, NA)
            game_winner <- coalesce(r[[y]]$details$properties$isGameWinningGoal, NA)
            insurance <- coalesce(r[[y]]$details$properties$isInsuranceGoal, NA)
            penalty_shot <- coalesce(r[[y]]$details$properties$isPenaltyShot, NA)
            power_play <- coalesce(r[[y]]$details$properties$isPowerPlay, NA)
            short_handed <- coalesce(r[[y]]$details$properties$isShortHanded, NA)

            x_loc <- coalesce(r[[y]]$details$xLocation, NA)
            y_loc <- coalesce(r[[y]]$details$yLocation, NA)

            if (as.numeric(period_of_game) > 3) {
              plus_player_one_id = coalesce(r[[y]]$details$plus_players[[1]]$id, NA)
              plus_player_one_first = coalesce(r[[y]]$details$plus_players[[1]]$firstName, NA)
              plus_player_one_last = coalesce(r[[y]]$details$plus_players[[1]]$lastName, NA)
              plus_player_one_position = coalesce(r[[y]]$details$plus_players[[1]]$position, NA)
              plus_player_two_id = coalesce(r[[y]]$details$plus_players[[2]]$id, NA)
              plus_player_two_first = coalesce(r[[y]]$details$plus_players[[2]]$firstName, NA)
              plus_player_two_last = coalesce(r[[y]]$details$plus_players[[2]]$lastName, NA)
              plus_player_two_position = coalesce(r[[y]]$details$plus_players[[2]]$position, NA)
              plus_player_three_id = coalesce(r[[y]]$details$plus_players[[3]]$id, NA)
              plus_player_three_first = coalesce(r[[y]]$details$plus_players[[3]]$firstName, NA)
              plus_player_three_last = coalesce(r[[y]]$details$plus_players[[3]]$lastName, NA)
              plus_player_three_position = coalesce(r[[y]]$details$plus_players[[3]]$position, NA)
              plus_player_four_id = coalesce(NA, NA)
              plus_player_four_first = coalesce(NA, NA)
              plus_player_four_last = coalesce(NA, NA)
              plus_player_four_position = coalesce(NA, NA)
              plus_player_five_id = coalesce(NA, NA)
              plus_player_five_first = coalesce(NA, NA)
              plus_player_five_last = coalesce(NA, NA)
              plus_player_five_position = coalesce(NA, NA)
              minus_player_one_id = coalesce(r[[y]]$details$minus_players[[1]]$id, NA)
              minus_player_one_first = coalesce(r[[y]]$details$minus_players[[1]]$firstName, NA)
              minus_player_one_last = coalesce(r[[y]]$details$minus_players[[1]]$lastName, NA)
              minus_player_one_position = coalesce(r[[y]]$details$minus_players[[1]]$position, NA)
              minus_player_two_id = coalesce(r[[y]]$details$minus_players[[2]]$id, NA)
              minus_player_two_first = coalesce(r[[y]]$details$minus_players[[2]]$firstName, NA)
              minus_player_two_last = coalesce(r[[y]]$details$minus_players[[2]]$lastName, NA)
              minus_player_two_position = coalesce(r[[y]]$details$minus_players[[2]]$position, NA)
              minus_player_three_id = coalesce(r[[y]]$details$minus_players[[3]]$id, NA)
              minus_player_three_first = coalesce(r[[y]]$details$minus_players[[3]]$firstName, NA)
              minus_player_three_last = coalesce(r[[y]]$details$minus_players[[3]]$lastName, NA)
              minus_player_three_position = coalesce(r[[y]]$details$minus_players[[3]]$position, NA)
              minus_player_four_id = coalesce(NA, NA)
              minus_player_four_first = coalesce(NA, NA)
              minus_player_four_last = coalesce(NA, NA)
              minus_player_four_position = coalesce(NA, NA)
              minus_player_five_id = coalesce(NA, NA)
              minus_player_five_first = coalesce(NA, NA)
              minus_player_five_last = coalesce(NA, NA)
              minus_player_five_position = coalesce(NA, NA)
            } else {
              plus_player_one_id = coalesce(r[[y]]$details$plus_players[[1]]$id, NA)
              plus_player_one_first = coalesce(r[[y]]$details$plus_players[[1]]$firstName, NA)
              plus_player_one_last = coalesce(r[[y]]$details$plus_players[[1]]$lastName, NA)
              plus_player_one_position = coalesce(r[[y]]$details$plus_players[[1]]$position, NA)
              plus_player_two_id = coalesce(r[[y]]$details$plus_players[[2]]$id, NA)
              plus_player_two_first = coalesce(r[[y]]$details$plus_players[[2]]$firstName, NA)
              plus_player_two_last = coalesce(r[[y]]$details$plus_players[[2]]$lastName, NA)
              plus_player_two_position = coalesce(r[[y]]$details$plus_players[[2]]$position, NA)
              plus_player_three_id = coalesce(r[[y]]$details$plus_players[[3]]$id, NA)
              plus_player_three_first = coalesce(r[[y]]$details$plus_players[[3]]$firstName, NA)
              plus_player_three_last = coalesce(r[[y]]$details$plus_players[[3]]$lastName, NA)
              plus_player_three_position = coalesce(r[[y]]$details$plus_players[[3]]$position, NA)
              plus_player_four_id = coalesce(r[[y]]$details$plus_players[[4]]$id, NA)
              plus_player_four_first = coalesce(r[[y]]$details$plus_players[[4]]$firstName, NA)
              plus_player_four_last = coalesce(r[[y]]$details$plus_players[[4]]$lastName, NA)
              plus_player_four_position = coalesce(r[[y]]$details$plus_players[[4]]$position, NA)
              plus_player_five_id = coalesce(r[[y]]$details$plus_players[[5]]$id, NA)
              plus_player_five_first = coalesce(r[[y]]$details$plus_players[[5]]$firstName, NA)
              plus_player_five_last = coalesce(r[[y]]$details$plus_players[[5]]$lastName, NA)
              plus_player_five_position = coalesce(r[[y]]$details$plus_players[[5]]$position, NA)
              minus_player_one_id = coalesce(r[[y]]$details$minus_players[[1]]$id, NA)
              minus_player_one_first = coalesce(r[[y]]$details$minus_players[[1]]$firstName, NA)
              minus_player_one_last = coalesce(r[[y]]$details$minus_players[[1]]$lastName, NA)
              minus_player_one_position = coalesce(r[[y]]$details$minus_players[[1]]$position, NA)
              minus_player_two_id = coalesce(r[[y]]$details$minus_players[[2]]$id, NA)
              minus_player_two_first = coalesce(r[[y]]$details$minus_players[[2]]$firstName, NA)
              minus_player_two_last = coalesce(r[[y]]$details$minus_players[[2]]$lastName, NA)
              minus_player_two_position = coalesce(r[[y]]$details$minus_players[[2]]$position, NA)
              minus_player_three_id = coalesce(r[[y]]$details$minus_players[[3]]$id, NA)
              minus_player_three_first = coalesce(r[[y]]$details$minus_players[[3]]$firstName, NA)
              minus_player_three_last = coalesce(r[[y]]$details$minus_players[[3]]$lastName, NA)
              minus_player_three_position = coalesce(r[[y]]$details$minus_players[[3]]$position, NA)
              minus_player_four_id = coalesce(r[[y]]$details$minus_players[[4]]$id, NA)
              minus_player_four_first = coalesce(r[[y]]$details$minus_players[[4]]$firstName, NA)
              minus_player_four_last = coalesce(r[[y]]$details$minus_players[[4]]$lastName, NA)
              minus_player_four_position = coalesce(r[[y]]$details$minus_players[[4]]$position, NA)
              minus_player_five_id = coalesce(r[[y]]$details$minus_players[[5]]$id, NA)
              minus_player_five_first = coalesce(r[[y]]$details$minus_players[[5]]$firstName, NA)
              minus_player_five_last = coalesce(r[[y]]$details$minus_players[[5]]$lastName, NA)
              minus_player_five_position = coalesce(r[[y]]$details$minus_players[[5]]$position, NA)
            }

            goal <- data.frame(
              event = c(event),
              team_id = c(team_id),
              period_of_game = c(period_of_game),
              time_of_period = c(time_of_period),
              player_id = c(scorer_id),
              player_name_first = c(scorer_first),
              player_name_last = c(scorer_last),
              player_position = c(scorer_pos),
              player_team_id = c(team_id),
              player_two_id = c(primary_assist_id),
              player_two_name_first = c(primary_assist_first),
              player_two_name_last = c(primary_assist_last),
              player_two_position = c(primary_assist_pos),
              player_three_id = c(sec_assist_id),
              player_three_name_first = c(sec_assist_first),
              player_three_name_last = c(sec_assist_last),
              player_three_position = c(sec_assist_pos),
              x_coord = c(x_loc),
              y_coord = c(y_loc),
              empty_net = c(empty_net),
              game_winner = c(game_winner),
              penalty_shot = c(penalty_shot),
              insurance = c(insurance),
              power_play = c(power_play),
              short_handed = c(short_handed),
              plus_player_one_id = c(plus_player_one_id),
              plus_player_one_first = c(plus_player_one_first),
              plus_player_one_last = c(plus_player_one_last),
              plus_player_one_position = c(plus_player_one_position),
              plus_player_two_id = c(plus_player_two_id),
              plus_player_two_first = c(plus_player_two_first),
              plus_player_two_last = c(plus_player_two_last),
              plus_player_two_position = c(plus_player_two_position),
              plus_player_three_id = c(plus_player_three_id),
              plus_player_three_first = c(plus_player_three_first),
              plus_player_three_last = c(plus_player_three_last),
              plus_player_three_position = c(plus_player_three_position),
              plus_player_four_id = c(plus_player_four_id),
              plus_player_four_first = c(plus_player_four_first),
              plus_player_four_last = c(plus_player_four_last),
              plus_player_four_position = c(plus_player_four_position),
              plus_player_five_id = c(plus_player_five_id),
              plus_player_five_first = c(plus_player_five_first),
              plus_player_five_last = c(plus_player_five_last),
              plus_player_five_position = c(plus_player_five_position),
              minus_player_one_id = c(minus_player_one_id),
              minus_player_one_first = c(minus_player_one_first),
              minus_player_one_last = c(minus_player_one_last),
              minus_player_one_position = c(minus_player_one_position),
              minus_player_two_id = c(minus_player_two_id),
              minus_player_two_first = c(minus_player_two_first),
              minus_player_two_last = c(minus_player_two_last),
              minus_player_two_position = c(minus_player_two_position),
              minus_player_three_id = c(minus_player_three_id),
              minus_player_three_first = c(minus_player_three_first),
              minus_player_three_last = c(minus_player_three_last),
              minus_player_three_position = c(minus_player_three_position),
              minus_player_four_id = c(minus_player_four_id),
              minus_player_four_first = c(minus_player_four_first),
              minus_player_four_last = c(minus_player_four_last),
              minus_player_four_position = c(minus_player_four_position),
              minus_player_five_id = c(minus_player_five_id),
              minus_player_five_first = c(minus_player_five_first),
              minus_player_five_last = c(minus_player_five_last),
              minus_player_five_position = c(minus_player_five_position)
            )

            game_events <- bind_rows(game_events, goal)

          } else if (event == "faceoff") {
            home_player_id <- coalesce(r[[y]]$details$homePlayer$id, NA)
            home_player_first <- coalesce(r[[y]]$details$homePlayer$firstName, NA)
            home_player_last <- coalesce(r[[y]]$details$homePlayer$lastName, NA)
            home_player_pos <- coalesce(r[[y]]$details$homePlayer$position, NA)

            away_player_id <- coalesce(r[[y]]$details$homePlayer$id, NA)
            away_player_first <- coalesce(r[[y]]$details$homePlayer$firstName, NA)
            away_player_last <- coalesce(r[[y]]$details$homePlayer$lastName, NA)
            away_player_pos <- coalesce(r[[y]]$details$homePlayer$position, NA)

            home_win <- coalesce(r[[y]]$details$homeWin, NA)
            x_loc <- coalesce(r[[y]]$details$xLocation, NA)
            y_loc <- coalesce(r[[y]]$details$yLocation, NA)

            if (as.numeric(home_win) == 1) {

              player_one_id <- coalesce(r[[y]]$details$homePlayer$id, NA)
              player_one_first <- coalesce(r[[y]]$details$homePlayer$firstName, NA)
              player_one_last <- coalesce(r[[y]]$details$homePlayer$lastName, NA)
              player_one_pos <- coalesce(r[[y]]$details$homePlayer$position, NA)

              player_two_id <- coalesce(r[[y]]$details$awayPlayer$id, NA)
              player_two_first <- coalesce(r[[y]]$details$awayPlayer$firstName, NA)
              player_two_last <- coalesce(r[[y]]$details$awayPlayer$lastName, NA)
              player_two_pos <- coalesce(r[[y]]$details$awayPlayer$position, NA)

            } else {

              player_two_id <- coalesce(r[[y]]$details$awayPlayer$id, NA)
              player_two_first <- coalesce(r[[y]]$details$awayPlayer$firstName, NA)
              player_two_last <- coalesce(r[[y]]$details$awayPlayer$lastName, NA)
              player_two_pos <- coalesce(r[[y]]$details$awayPlayer$position, NA)

              player_one_id <- coalesce(r[[y]]$details$homePlayer$id, NA)
              player_one_first <- coalesce(r[[y]]$details$homePlayer$firstName, NA)
              player_one_last <- coalesce(r[[y]]$details$homePlayer$lastName, NA)
              player_one_pos <- coalesce(r[[y]]$details$homePlayer$position, NA)

            }

            faceoff <- data.frame(
              event = c(event),
              team_id = c(team_id),
              period_of_game = c(period_of_game),
              time_of_period = c(time_of_period),
              player_id = c(player_one_id),
              player_name_first = c(player_one_first),
              player_name_last = c(player_one_last),
              player_position = c(player_one_pos),
              # player_team_id = c(team_id),
              player_two_id = c(player_two_id),
              player_two_name_first = c(player_two_first),
              player_two_name_last = c(player_two_last),
              player_two_position = c(player_two_pos),
              x_coord = c(x_loc),
              y_coord = c(y_loc),
              home_win = c(as.numeric(home_win))
            )

            game_events <- bind_rows(game_events, faceoff)

          } else if (event  == "penalty") {

            penalty_type <- coalesce(r[[y]]$details$description, NA)
            penalty_length <- coalesce(r[[y]]$details$minutes, NA)
            starts_pp <- coalesce(r[[y]]$details, NA)

            taken_id <- coalesce(r[[y]]$details$takenBy$id, NA)
            taken_first <- coalesce(r[[y]]$details$takenBy$firstName, NA)
            taken_last <- coalesce(r[[y]]$details$takenBy$lastName, NA)
            taken_pos <- coalesce(r[[y]]$details$takenBy$position, NA)

            served_id <- coalesce(r[[y]]$details$servedBy$id, NA)
            served_first <- coalesce(r[[y]]$details$servedBy$firstName, NA)
            served_last <- coalesce(r[[y]]$details$servedBy$lastName, NA)
            served_pos <- coalesce(r[[y]]$details$servedBy$position, NA)

            against_tm_id <- coalesce(r[[y]]$details$againstTeam$id, NA)
            against_tm_abbr <- coalesce(r[[y]]$details$againstTeam$abbreviation, NA)

            penalty <- data.frame(
              event = c(event),
              team_id = c(team_id),
              period_of_game = c(period_of_game),
              time_of_period = c(time_of_period),
              player_id = c(served_id),
              player_name_first = c(served_first),
              player_name_last = c(served_last),
              player_position = c(served_pos),
              # player_team_id = c(team_id),
              player_two_id = c(taken_id),
              player_two_name_first = c(taken_first),
              player_two_name_last = c(taken_last),
              player_two_position = c(taken_pos),
              event_type = c(penalty_type),
              penalty_length = c(penalty_length),
              power_play = c(starts_pp)
            )

            game_events <- bind_rows(game_events, penalty)

          }

        print(paste0(y))

      }

      game_df <- game_events %>%
        dplyr::select(-contains(".")) %>%
        dplyr::mutate(game_id = game_id) %>%
        dplyr::relocate(game_id, .before = c(1))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid season or no roster data available! Try a season from 2023 onwards!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(game_df)

}
