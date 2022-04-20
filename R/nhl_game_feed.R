#' @title **NHL Game Feed**
#' @description Returns information on game feed for a given game id
#' @param game_id Game unique ID
#' @return Returns a named list of data frames: all_plays, scoring_plays,
#' penalty_plays, plays_by_period, current_play, linescore, decisions,
#' team_box, player_box, skaters, goalies, on_ice, on_ice_plus,
#' penalty_box, scratches, team_coaches
#' @keywords NHL Game Feed
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'    try(nhl_game_feed(game_id=2018020561))
#' }
nhl_game_feed <- function(game_id){

  base_url <- "https://statsapi.web.nhl.com/api/v1/game/"

  full_url <- paste0(base_url,
                     game_id,
                     "/feed/live")


  tryCatch(
    expr = {
      res <- httr::RETRY("GET", full_url)

      # Check the result
      check_status(res)

      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      #---Live Data----
      live_data_df <- jsonlite::fromJSON(resp)[["liveData"]]
      plays_df <- live_data_df$plays$allPlays
      plays_player <- jsonlite::fromJSON(jsonlite::toJSON(plays_df$players),flatten=TRUE)
      plays_df <- jsonlite::fromJSON(jsonlite::toJSON(plays_df),flatten=TRUE)
      if(length(plays_player)>0){
        plays_player <- purrr::map_dfr(1:length(plays_player), function(x){
          if("playerType" %in% colnames(plays_player[[x]]) && nrow(plays_player[[x]])>1){

            player <- data.frame(
              "playerType_1" = plays_player[[x]]$playerType[[1]],
              "player.id_1" = plays_player[[x]]$player.id[[1]],
              "player.fullName_1" = plays_player[[x]]$player.fullName[[1]],
              "player.link_1" = plays_player[[x]]$player.link[[1]],
              "playerType_2" = plays_player[[x]]$playerType[[2]],
              "player.id_2" = plays_player[[x]]$player.id[[2]],
              "player.fullName_2" = plays_player[[x]]$player.fullName[[2]],
              "player.link_2" = plays_player[[x]]$player.link[[2]])
            return(player)
          }else if("playerType" %in% colnames(plays_player[[x]]) && nrow(plays_player[[x]])==1){

            player <- data.frame(
              "playerType_1" = plays_player[[x]]$playerType[[1]],
              "player.id_1" = plays_player[[x]]$player.id[[1]],
              "player.fullName_1" = plays_player[[x]]$player.fullName[[1]],
              "player.link_1" = plays_player[[x]]$player.link[[1]],
              "playerType_2" = NA_character_,
              "player.id_2" = NA_integer_,
              "player.fullName_2" = NA_character_,
              "player.link_2" = NA_character_)
            return(player)
          }else{
            player <- data.frame(
              "playerType_1" = NA_character_,
              "player.id_1" = NA_integer_,
              "player.fullName_1" = NA_character_,
              "player.link_1" = NA_character_,
              "playerType_2" = NA_character_,
              "player.id_2" = NA_integer_,
              "player.fullName_2" = NA_character_,
              "player.link_2" = NA_character_)
            return(player)
          }
        })
      }
      if(length(plays_df)>0 && length(plays_player)>0){
        all_plays <- plays_df %>%
          dplyr::select(-.data$players) %>%
          dplyr::bind_cols(plays_player) %>%
          janitor::clean_names() %>%
          make_fastRhockey_data("NHL Game Plays Information from NHL.com",Sys.time())
      }else{
        all_plays = data.frame()
      }
      scoring_plays <- live_data_df$plays$scoringPlays %>%
        make_fastRhockey_data("NHL Game Scoring Plays Information from NHL.com",Sys.time())
      penalty_plays <- live_data_df$plays$penaltyPlays %>%
        make_fastRhockey_data("NHL Game Penalty Box Information from NHL.com",Sys.time())
      plays_by_period <- live_data_df$plays$playsByPeriod %>%
        make_fastRhockey_data("NHL Game Plays by Period Information from NHL.com",Sys.time())
      current_play <- live_data_df$plays$currentPlay
      linescore <- live_data_df$linescore

      decisions <- live_data_df$decisions %>%
        make_fastRhockey_data("NHL Game Decisions Information from NHL.com",Sys.time())
      ##-- boxscore ----
      ###---officials----
      officials_df <- live_data_df$boxscore[["officials"]]
      if(length(officials_df) > 1){
        officials_df <- jsonlite::fromJSON(jsonlite::toJSON(officials_df),flatten=TRUE) %>%
          janitor::clean_names() %>%
          make_fastRhockey_data("NHL Game Officials Information from NHL.com",Sys.time())
      }
      game_boxscore_df <- live_data_df$boxscore[["teams"]]
      game_boxscore_df <- jsonlite::fromJSON(jsonlite::toJSON(game_boxscore_df),flatten=TRUE)
      ###---team_box----
      away_boxscore <- game_boxscore_df[["away"]]
      home_boxscore <- game_boxscore_df[["home"]]
      away_team_box <- away_boxscore$team %>%
        dplyr::bind_cols(away_boxscore$teamStats$teamSkaterStats)
      home_team_box <- home_boxscore$team %>%
        dplyr::bind_cols(home_boxscore$teamStats$teamSkaterStats)
      team_box <- dplyr::bind_rows(away_team_box, home_team_box) %>%
        dplyr::rename(
          team_id = .data$id,
          team_name = .data$name) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Game Team Box Information from NHL.com",Sys.time())
      ###---player_box----
      away_players_box <- purrr::map_df(1:length(away_boxscore$players),function(x){
        person <- data.frame(away_boxscore$players[[x]][["person"]]) %>%
          janitor::clean_names()
        jersey <- data.frame("jerseyNumber" = away_boxscore$players[[x]][["jerseyNumber"]]) %>%
          janitor::clean_names()
        position <- data.frame(away_boxscore$players[[x]][["position"]]) %>%
          janitor::clean_names()
        player_stats <- data.frame(away_boxscore$players[[x]][["stats"]]) %>%
          janitor::clean_names()
        away_player_boxscore <-dplyr::bind_cols(person,jersey,position,player_stats)
        return(away_player_boxscore)
      })
      home_players_box <- purrr::map_df(1:length(home_boxscore$players),function(x){
        person <- data.frame(home_boxscore$players[[x]][["person"]]) %>%
          janitor::clean_names()
        jersey <- data.frame("jerseyNumber" = home_boxscore$players[[x]][["jerseyNumber"]]) %>%
          janitor::clean_names()
        position <- data.frame(home_boxscore$players[[x]][["position"]]) %>%
          janitor::clean_names()
        player_stats <- data.frame(home_boxscore$players[[x]][["stats"]]) %>%
          janitor::clean_names()
        home_player_boxscore <-dplyr::bind_cols(person,jersey,position,player_stats)
        return(home_player_boxscore)
      })
      away_players_box$home_away <- "Away"
      home_players_box$home_away <- "Home"
      players_box <- dplyr::bind_rows(away_players_box, home_players_box) %>%
        dplyr::rename(
          player_id = .data$id,
          player_full_name = .data$full_name,
          position_code = .data$code,
          position_name = .data$name,
          position_type = .data$type,
          position_abbreviation = .data$abbreviation) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Game Players Box Information from NHL.com",Sys.time())

      ###---goalies----
      away_goalies <- data.frame("goalies" = away_boxscore$goalies)
      home_goalies <- data.frame("goalies" = home_boxscore$goalies)
      away_goalies$home_away <- "Away"
      home_goalies$home_away <- "Home"
      goalies <- dplyr::bind_rows(away_goalies,home_goalies) %>%
        make_fastRhockey_data("NHL Game Goalies Information from NHL.com",Sys.time())
      ###---skaters----
      away_skaters <- data.frame("skaters" = away_boxscore$skaters)
      home_skaters <- data.frame("skaters" = home_boxscore$skaters)
      away_skaters$home_away <- "Away"
      home_skaters$home_away <- "Home"
      skaters <- dplyr::bind_rows(away_skaters,home_skaters) %>%
        make_fastRhockey_data("NHL Game Skaters Information from NHL.com",Sys.time())
      ###---onIce----
      away_onIce <- data.frame("onIce" = away_boxscore$onIce)
      home_onIce <- data.frame("onIce" = home_boxscore$onIce)
      onIce <- dplyr::bind_rows(away_onIce,home_onIce) %>%
        make_fastRhockey_data("NHL Game On Ice Information from NHL.com",Sys.time())
      ###---onIcePlus----
      away_onIcePlus <- data.frame("onIcePlus" = away_boxscore$onIcePlus)
      home_onIcePlus <- data.frame("onIcePlus" = home_boxscore$onIcePlus)
      onIcePlus <- dplyr::bind_rows(away_onIcePlus,home_onIcePlus) %>%
        make_fastRhockey_data("NHL Game On Ice+ Information from NHL.com",Sys.time())
      ###---penaltyBox----
      away_penaltyBox <- data.frame("penaltyBox" = away_boxscore$penaltyBox)
      home_penaltyBox <- data.frame("penaltyBox" = home_boxscore$penaltyBox)
      penaltyBox <- dplyr::bind_rows(away_penaltyBox,home_penaltyBox) %>%
        make_fastRhockey_data("NHL Game Penalty Box Information from NHL.com",Sys.time())
      ###---scratches----
      away_scratches <- data.frame("scratches" = away_boxscore$scratches)
      home_scratches <- data.frame("scratches" = home_boxscore$scratches)
      scratches <- dplyr::bind_rows(away_scratches,home_scratches) %>%
        make_fastRhockey_data("NHL Game Scratches Information from NHL.com",Sys.time())
      ###---coaches----
      away_coaches <- away_boxscore$coaches
      home_coaches <- home_boxscore$coaches
      away_coaches$home_away <- "Away"
      home_coaches$home_away <- "Home"
      team_coaches <- dplyr::bind_rows(away_coaches, home_coaches) %>%
        make_fastRhockey_data("NHL Game Team Coaches Information from NHL.com",Sys.time())
      ###---
      game = c(list(all_plays),list(scoring_plays), list(penalty_plays),list(plays_by_period),
               list(current_play), list(linescore), list(decisions),
               list(team_box),list(players_box), list(skaters), list(goalies), list(onIce),
               list(onIcePlus), list(penaltyBox), list(scratches), list(team_coaches))
      names(game) <- c("all_plays", "scoring_plays", "penalty_plays", "plays_by_period",
                       "current_play", "linescore", "decisions",
                       "team_box", "player_box", "skaters", "goalies", "on_ice", "on_ice_plus",
                       "penalty_box", "scratches", "team_coaches")
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no game feed data for {game_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(game)
}
