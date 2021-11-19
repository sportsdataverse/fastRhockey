#' @title **NHL Game Boxscore**
#' @description Returns information on game boxscore for a given game id
#' @param game_id Game unique ID 
#' @return Returns a named list of data frames: team_box, player_box, 
#' skaters, goalies, on_ice, on_ice_plus, penalty_box, 
#' scratches, team_coaches
#' @keywords NHL Game Boxscore
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples 
#' \donttest{
#'   nhl_game_boxscore(game_id=2021020182)
#' }
nhl_game_boxscore <- function(game_id){
  
  base_url <- "https://statsapi.web.nhl.com/api/v1/game/"
  
  full_url <- paste0(base_url, 
                     game_id,
                     "/boxscore")
  
  
  res <- httr::RETRY("GET", full_url)
  
  # Check the result
  check_status(res)
  
  resp <- res %>%
    httr::content(as = "text", encoding = "UTF-8")
  tryCatch(
    expr = {
      #---officials----
      officials_df <- jsonlite::fromJSON(resp)[["officials"]]
      officials_df <- jsonlite::fromJSON(jsonlite::toJSON(officials_df),flatten=TRUE) %>% 
        janitor::clean_names()
      game_boxscore_df <- jsonlite::fromJSON(resp)[["teams"]]
      game_boxscore_df <- jsonlite::fromJSON(jsonlite::toJSON(game_boxscore_df),flatten=TRUE)
      #---team_box----
      away_boxscore <- game_boxscore_df[["away"]]
      home_boxscore <- game_boxscore_df[["home"]]
      away_team_box <- away_boxscore$team %>% 
        dplyr::bind_cols(away_boxscore$teamStats$teamSkaterStats)
      home_team_box <- home_boxscore$team %>% 
        dplyr::bind_cols(home_boxscore$teamStats$teamSkaterStats)
      team_box <- dplyr::bind_rows(away_team_box, home_team_box) %>% 
        dplyr::rename(
          team_id = .data$id,
          team_name = .data$name)
      #---player_box----
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
        janitor::clean_names()
      
      #---goalies----
      away_goalies <- data.frame("goalies" = away_boxscore$goalies)
      home_goalies <- data.frame("goalies" = home_boxscore$goalies)
      away_goalies$home_away <- "Away"
      home_goalies$home_away <- "Home"
      goalies <- dplyr::bind_rows(away_goalies,home_goalies)
      #---skaters----
      away_skaters <- data.frame("skaters" = away_boxscore$skaters)
      home_skaters <- data.frame("skaters" = home_boxscore$skaters)
      away_skaters$home_away <- "Away"
      home_skaters$home_away <- "Home"
      skaters <- dplyr::bind_rows(away_skaters,home_skaters)
      #---onIce----
      away_onIce <- data.frame("onIce" = away_boxscore$onIce)
      home_onIce <- data.frame("onIce" = home_boxscore$onIce)
      away_onIce$home_away <- "Away"
      home_onIce$home_away <- "Home"
      onIce <- dplyr::bind_rows(away_onIce,home_onIce)
      #---onIcePlus----
      away_onIcePlus <- data.frame("onIcePlus" = away_boxscore$onIcePlus)
      home_onIcePlus <- data.frame("onIcePlus" = home_boxscore$onIcePlus)
      away_onIcePlus$home_away <- "Away"
      home_onIcePlus$home_away <- "Home"
      onIcePlus <- dplyr::bind_rows(away_onIcePlus,home_onIcePlus)
      #---penaltyBox----
      away_penaltyBox <- data.frame("penaltyBox" = away_boxscore$penaltyBox)
      home_penaltyBox <- data.frame("penaltyBox" = home_boxscore$penaltyBox)
      penaltyBox <- dplyr::bind_rows(away_penaltyBox,home_penaltyBox)
      #---scratches----
      away_scratches <- data.frame("scratches" = away_boxscore$scratches)
      home_scratches <- data.frame("scratches" = home_boxscore$scratches)
      away_scratches$home_away <- "Away"
      home_scratches$home_away <- "Home"
      scratches <- dplyr::bind_rows(away_scratches,home_scratches)
      #---coaches----
      away_coaches <- away_boxscore$coaches
      home_coaches <- home_boxscore$coaches
      away_coaches$home_away <- "Away"
      home_coaches$home_away <- "Home"
      team_coaches <- dplyr::bind_rows(away_coaches, home_coaches)
      #---
      game = c(list(team_box),list(players_box), list(skaters), list(goalies), list(onIce),
               list(onIcePlus), list(penaltyBox), list(scratches), list(team_coaches))
      names(game) <- c("team_box","player_box","skaters","goalies", "on_ice", "on_ice_plus",
                       "penalty_box", "scratches", "team_coaches")
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no game boxscore data for {game_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(game)
}