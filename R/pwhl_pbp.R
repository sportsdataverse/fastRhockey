
require(tidyverse)

game_id <- 27
URL <- paste0("https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=gameCenterPlayByPlay&game_id=", game_id, "&key=694cfeed58c932ee&client_code=pwhl&lang=en&league_id=&callback=angular.callbacks._8")

res <- httr::RETRY("GET", URL)
res <- res %>%
  httr::content(as = "text", encoding = "utf-8")
res <- gsub("angular.callbacks._8\\(", "", res)
res <- gsub("}}])", "}}]", res)

r <- res %>% jsonlite::parse_json()

# events <- "goalie_change"
events <- c("goalie_change", "faceoff", "shot", "penalty", "goal")

for (y in 0:length(r)) {
  # if (! r[[y]]$event %in% events) {
  #   events <- append(events, r[[y]]$event)
  # }

  event <- r[[y]]$event
  team_id <- r[[y]]$details$team_id
  period_of_game <- r[[y]]$details$period$id
  time_of_period <- r[[y]]$details$time

  if (event == "goalie_change") {

    player_in_id <- r[[y]]$details$goalieComingIn$id
    player_in_first <- r[[y]]$details$goalieComingIn$firstName
    player_in_last <- r[[y]]$details$goalieComingIn$lastName
    player_out <- r[[y]]$details$goalieGoingOut

    if (! is.null(player_out)) {

      player_out_id <- r[[y]]$details$goalieGoingOut$id
      player_out_first <- r[[y]]$details$goalieGoingOut$firstName
      player_out_last <- r[[y]]$details$goalieGoingOut$lastName

    } else {
      player_out_id <- NULL
      player_out_first <- NULL
      player_out_last <- NULL
    }

    if (event == "shot") {
      shooter_id <- r[[y]]$details$shooter$id
      shooter_first <- r[[y]]$details$shooter$firstName
      shooter_last <- r[[y]]$details$shooter$lastName
      shooter_jersey <- r[[y]]$details$shooter$jerseyNumber
      shooter_position <- r[[y]]$details$shooter$position

      goal <- r[[y]]$details$isGoal

      goalie_id <- r[[y]]$details$goalie$id
      goalie_first <- r[[y]]$details$goalie$firstName
      goalie_last <- r[[y]]$details$goalie$lastName

      team_id <- r[[y]]$details$shooterTeamId
      shot_quality <- r[[y]]$details$shotQuality
      shot_type <- r[[y]]$details$shotType
      x_loc <- r[[y]]$details$xLocation
      y_loc <- r[[y]]$details$yLocation
    }

    if (event == "goal") {
      scorer_id <- r[[y]]$details$scoredBy$id
      scorer_first <- r[[y]]$details$scoredBy$firstName
      scorer_last <- r[[y]]$details$scoredBy$lastName
      scorer_pos <- r[[y]]$details$scoredBy$position

      primary_assist_id <- r[[y]]$details$assists[[1]]$id
      primary_assist_first <- r[[y]]$details$assists[[1]]$firstName
      primary_assist_last <- r[[y]]$details$assists[[1]]$lastName
      primary_assist_pos <- r[[y]]$details$assists[[1]]$position

      if (as.numeric(r[[y]]$details$assistNumbers[[2]]) > 0) {

        sec_assist_id <- r[[y]]$details$assists[[2]]$id
        sec_assist_first <- r[[y]]$details$assists[[2]]$firstName
        sec_assist_last <- r[[y]]$details$assists[[2]]$lastName
        sec_assist_pos <- r[[y]]$details$assists[[2]]$position

      } else {

        sec_assist_id <- NULL
        sec_assist_first <- NULL
        sec_assist_last <- NULL
        sec_assist_pos <- NULL

      }

      empty_net <- r[[y]]$details$properties$isEmptyNet
      game_winner <- r[[y]]$details$properties$isGameWinningGoal
      insurance <- r[[y]]$details$properties$isInsuranceGoal
      penalty_shot <- r[[y]]$details$properties$isPenaltyShot
      power_play <- r[[y]]$details$properties$isPowerPlay
      short_handed <- r[[y]]$details$properties$isShortHanded

      x_loc <- r[[y]]$details$xLocation
      y_loc <- r[[y]]$details$yLocation

    }

    if (event == "faceoff") {
      home_player_id <- r[[y]]$details$homePlayer$id
      home_player_first <- r[[y]]$details$homePlayer$firstName
      home_player_last <- r[[y]]$details$homePlayer$lastName
      home_player_pos <- r[[y]]$details$homePlayer$position

      away_player_id <- r[[y]]$details$homePlayer$id
      away_player_first <- r[[y]]$details$homePlayer$firstName
      away_player_last <- r[[y]]$details$homePlayer$lastName
      away_player_pos <- r[[y]]$details$homePlayer$position

      home_win <- r[[y]]$details$homeWin
      x_loc <- r[[y]]$details$xLocation
      y_loc <- r[[y]]$details$yLocation

    }

  }

}
