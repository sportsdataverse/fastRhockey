#' @title  **PWHL Play-by-play**
#' @description PWHL Play-by-play
#'
#' @param game_id Game ID that you want play-by-play for
#' @return A data frame with play-by-play data from the PWHL
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples \donttest{
#'   try(pwhl_pbp(game_id = 27))
#' }

pwhl_pbp <- function(game_id) {

  URL <- glue::glue("https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=gameCenterPlayByPlay&game_id={game_id}&key=694cfeed58c932ee&client_code=pwhl&lang=en&league_id=&callback=angular.callbacks._8")

  res <- httr::RETRY("GET", URL)
  res <- res %>%
    httr::content(as = "text", encoding = "utf-8")
  callback_pattern <- "angular.callbacks._\\d+\\("
  res <- gsub(callback_pattern, "", res)
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

            assist <- r[[y]]$details$assists

            primary_assist_id <- if (length(assist) > 0) assist[[1]]$id else NA
            primary_assist_first <- if (length(assist) > 0) assist[[1]]$firstName else NA
            primary_assist_last <- if (length(assist) > 0) assist[[1]]$lastName else NA
            primary_assist_pos <- if (length(assist) > 0) assist[[1]]$position else NA

            sec_assist_id <- if (length(assist) > 1) assist[[2]]$id else NA
            sec_assist_first <- if (length(assist) > 1) assist[[2]]$firstName else NA
            sec_assist_last <- if (length(assist) > 1) assist[[2]]$lastName else NA
            sec_assist_pos <- if (length(assist) > 1) assist[[2]]$position else NA

            empty_net <- coalesce(r[[y]]$details$properties$isEmptyNet, NA)
            game_winner <- coalesce(r[[y]]$details$properties$isGameWinningGoal, NA)
            insurance <- coalesce(r[[y]]$details$properties$isInsuranceGoal, NA)
            penalty_shot <- coalesce(r[[y]]$details$properties$isPenaltyShot, NA)
            power_play <- coalesce(r[[y]]$details$properties$isPowerPlay, NA)
            short_handed <- coalesce(r[[y]]$details$properties$isShortHanded, NA)

            x_loc <- coalesce(r[[y]]$details$xLocation, NA)
            y_loc <- coalesce(r[[y]]$details$yLocation, NA)

            minus <- r[[y]]$details$minus_players
            plus <- r[[y]]$details$plus_players

            plus_player_one_id = if (length(plus) > 0) plus[[1]]$id else NA
            plus_player_one_first = if (length(plus) > 0) plus[[1]]$firstName else NA
            plus_player_one_last = if (length(plus) > 0) plus[[1]]$lastName else NA
            plus_player_one_position = if (length(plus) > 0) plus[[1]]$position else NA
            plus_player_two_id = if (length(plus) > 1) plus[[2]]$id else NA
            plus_player_two_first = if (length(plus) > 1) plus[[2]]$firstName else NA
            plus_player_two_last = if (length(plus) > 1) plus[[2]]$lastName else NA
            plus_player_two_position = if (length(plus) > 1) plus[[2]]$position else NA
            plus_player_three_id = if (length(plus) > 2) plus[[3]]$id else NA
            plus_player_three_first = if (length(plus) > 2) plus[[3]]$firstName else NA
            plus_player_three_last = if (length(plus) > 2) plus[[3]]$lastName else NA
            plus_player_three_position = if (length(plus) > 2) plus[[3]]$position else NA
            plus_player_four_id = if (length(plus) > 3) plus[[4]]$id else NA
            plus_player_four_first = if (length(plus) > 3) plus[[4]]$firstName else NA
            plus_player_four_last = if (length(plus) > 3) plus[[4]]$lastName else NA
            plus_player_four_position = if (length(plus) > 3) plus[[4]]$lastName else NA
            plus_player_five_id = if (length(plus) > 4) plus[[5]]$id else NA
            plus_player_five_first = if (length(plus) > 4) plus[[5]]$firstName else NA
            plus_player_five_last = if (length(plus) > 4) plus[[5]]$lastName else NA
            plus_player_five_position = if (length(plus) > 4) plus[[5]]$position else NA
            #
            minus_player_one_id = if (length(minus) > 0) minus[[1]]$id else NA
            minus_player_one_first = if (length(minus) > 0) minus[[1]]$firstName else NA
            minus_player_one_last = if (length(minus) > 0) minus[[1]]$lastName else NA
            minus_player_one_position = if (length(minus) > 0) minus[[1]]$position else NA
            minus_player_two_id = if (length(minus) > 1) minus[[2]]$id else NA
            minus_player_two_first = if (length(minus) > 1) minus[[2]]$firstName else NA
            minus_player_two_last = if (length(minus) > 1) minus[[2]]$lastName else NA
            minus_player_two_position = if (length(minus) > 1) minus[[2]]$position else NA
            minus_player_three_id = if (length(minus) > 2) minus[[3]]$id else NA
            minus_player_three_first = if (length(minus) > 2) minus[[3]]$firstName else NA
            minus_player_three_last = if (length(minus) > 2) minus[[3]]$lastName else NA
            minus_player_three_position = if (length(minus) > 2) minus[[3]]$position else NA
            minus_player_four_id = if (length(minus) > 3) minus[[4]]$id else NA
            minus_player_four_first = if (length(minus) > 3) minus[[4]]$firstName else NA
            minus_player_four_last = if (length(minus) > 3) minus[[4]]$lastName else NA
            minus_player_four_position = if (length(minus) > 3) minus[[4]]$lastName else NA
            minus_player_five_id = if (length(minus) > 4) minus[[5]]$id else NA
            minus_player_five_first = if (length(minus) > 4) minus[[5]]$firstName else NA
            minus_player_five_last = if (length(minus) > 4) minus[[5]]$lastName else NA
            minus_player_five_position = if (length(minus) > 4) minus[[5]]$position else NA

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

            team_id <- as.numeric(coalesce(r[[y]]$details$againstTeam$id, NA))
            penalty_type <- coalesce(r[[y]]$details$description, NA)
            penalty_length <- coalesce(r[[y]]$details$minutes, NA)
            starts_pp <- if(r[[y]]$details$isPowerPlay) "1" else "0"

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

            if (! is.na(penalty$team_id)) {
              game_events <- bind_rows(game_events, penalty)
            }

          }

        # print(paste0(y))

      }

      id = game_id

      games <- pwhl_game_info(game_id = id)

      game_df <- game_events %>%
        dplyr::select(-contains(".")) %>%
        dplyr::mutate(game_id = as.numeric(game_id),
                      game_date = games$game_date,
                      game_season = games$game_season,
                      game_season_id = games$game_season_id,
                      power_play = as.numeric(power_play),
                      home_team_id = games$home_team_id,
                      home_team = games$home_team,
                      away_team_id = games$away_team_id,
                      away_team = games$away_team) %>%
        dplyr::relocate(game_id, .before = c(1)) %>%
        dplyr::mutate(x_coord_original = .data$x_coord,
                      y_coord_original = .data$y_coord,
                      x_coord_neutral = .data$x_coord - 300,
                      y_coord_neutral = .data$y_coord - 150,
                      x_coord = (.data$x_coord / 3) - 100,
                      y_coord = 42.5 - (((.data$y_coord * 85) / 300) - 42.5) - 42.5,
                      x_coord_fixed = .data$x_coord / 3,
                      y_coord_fixed = 42.5 - (((.data$y_coord * 85) / 300) - 42.5),
                      x_coord_right = ifelse(.data$team_id == .data$home_team_id, 100 + (100 - .data$x_coord), .data$x_coord),
                      y_coord_right = ifelse(.data$team_id == .data$home_team_id, 42.5 - (.data$y_coord - 42.5), .data$y_coord),
                      x_coord_vertical = 42.5 - (.data$y_coord_right - 42.5),
                      y_coord_vertical = .data$x_coord_right) %>%
        tidyr::separate("time_of_period", into = c("minute", "second"), sep = ":", remove = FALSE) %>%
        dplyr::mutate(
          minute_start = as.numeric(.data$minute),
          second_start = as.numeric(.data$second),
          minute = ifelse(19 - .data$minute_start == 19 &
                            60 - .data$second_start == 60, 20,
                          19 - .data$minute_start),
          second = ifelse(60 - .data$second_start == 60, 0,
                          60 - .data$second_start),
          second = ifelse(.data$second < 10, paste0("0", .data$second),
                          paste0(.data$second)),
          clock = paste0(.data$minute, ":", .data$second),
          sec_from_start = (60 * .data$minute_start) + .data$second_start,
          # adding time to the seconds_from_start variable to account for what period we're in
          sec_from_start = dplyr::case_when(
            .data$period_of_game == 2 ~ .data$sec_from_start + 1200,
            .data$period_of_game == 3 ~ .data$sec_from_start + 2400,
            .data$period_of_game == 4 ~ .data$sec_from_start + 3600,
            .data$period_of_game == 5 ~ .data$sec_from_start + 4800,
            TRUE ~ .data$sec_from_start),
          start_power_play = ifelse(.data$event == "penalty" & .data$power_play == 1, .data$sec_from_start, NA_real_),
          end_power_play = ifelse(.data$event == "penalty" & .data$power_play == 1, .data$sec_from_start + (60 * as.numeric(.data$penalty_length)), NA_real_)) %>%
        dplyr::select(-"minute", -"second")

      goals <- game_df %>%
        dplyr::filter(event == "goal") %>%
        dplyr::select(sec_from_start, team_id)

      pens <- game_df %>%
        dplyr::filter(event == "penalty") %>%
        dplyr::mutate(advantage_team = ifelse(.data$team_id == home_team_id, away_team_id, home_team_id)) %>%
        dplyr::select(power_play, sec_from_start, penalty_length, start_power_play, end_power_play, advantage_team, team_id)

      for (i in 1:nrow(pens)) {


        if (i == 1) {goal <- goals %>%
          dplyr::filter(sec_from_start >= pens[i, ]$start_power_play & sec_from_start <= pens[i, ]$end_power_play)
        } else {
          goal <- goals %>%
            dplyr::filter(sec_from_start >= pens[i, ]$start_power_play & sec_from_start <= pens[i, ]$end_power_play &
                            sec_from_start > pens[i - 1, ]$end_power_play)
        }

        if (nrow(goal) > 0) {

          pens[i, ]$end_power_play <- goal$sec_from_start[1]

        }

      }

      for (i in 1:nrow(game_df)) {

        play_pen <- pens %>% dplyr::filter(start_power_play <= game_df[i, ]$sec_from_start &
                                             end_power_play >= game_df[i, ]$sec_from_start &
                                             advantage_team == game_df[i, ]$team_id) %>%
          dplyr::slice(1)

        if (nrow(play_pen) > 0 & game_df[i,]$event %in% c("shot", "faceoff")) {
          game_df[i, ]$power_play <- if (game_df[i, ]$team_id == play_pen$advantage_team) 1 else 0
          game_df[i, ]$short_handed <- if (game_df[i, ]$team_id != play_pen$advantage_team) 1 else 0
        }

      }

      game_df <- game_df %>%
        dplyr::select(-c(start_power_play, end_power_play))

    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Error encountered: {e$message}. Please verify the game_id and ensure it corresponds to a valid game in the PWHL."))
    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(game_df)

}
