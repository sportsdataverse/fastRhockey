#' @title **PHF Game All**
#' @description phf_game_all: pull in the raw data for a game_id from the PHF/NWHL API
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @return A named list of data frames: plays, team_box, skaters, goalies, game_details,
#' scoring_summary, shootout_summary,
#' penalty_summary, officials, team_staff, timeouts
#' @import rvest
#' @import httr
#' @import dplyr
#' @importFrom jsonlite parse_json
#' @importFrom purrr pluck map_dfr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(phf_game_all(game_id = 268127))
#' }
phf_game_all <- function(game_id) {
  base_url <- "https://web.api.digitalshift.ca/partials/stats/game/play-by-play?game_id="
  full_url <- paste0(base_url, game_id)

  # setting the ticket as something that can be changed in case the API decides to change it's authorization
  # rather than hard-coding it in
  auth_ticket <- getOption(
    "fastRhockey.phf_ticket",
    default = 'ticket="4dM1QOOKk-PQTSZxW_zfXnOgbh80dOGK6eUb_MaSl7nUN0_k4LxLMvZyeaYGXQuLyWBOQhY8Q65k6_uwMu6oojuO"'
  )

  # the link for the game + authorization for accessing the API
  res <- httr::RETRY("GET", full_url,
                     httr::add_headers(`Authorization`= auth_ticket))
  # Check the result
  check_status(res)


  tryCatch(
    expr={
      data <- res %>%
        httr::content(as = "text", encoding="utf-8") %>%
        jsonlite::fromJSON() %>%
        purrr::pluck("content") %>%
        rvest::read_html() %>%
        rvest::html_table()

      plays_data <- data[
        !sapply(
          lapply(data, function(x){
            if("Time" %in% colnames(x) && nrow(x)>0){
              return(x)
            }
            if("Play" %in% colnames(x) && nrow(x)>0){
              return(x)
            }
          }),is.null)]
      if(length(plays_data) %in% c(5,6)){
        plays_data <- plays_data[1:5]
      } else if(length(plays_data)>6) {
        plays_data
      }
      plays_df <- purrr::map_dfr(1:length(plays_data), function(x){
        plays_data[[x]] %>%
          helper_phf_pbp_normalize_columns() %>%
          dplyr::mutate(
            period_id = x,
            game_id = game_id)
      })
      game_details <- phf_game_details(game_id)

      plays_df <- plays_df %>%
        dplyr::left_join(game_details, by = "game_id")

      plays_df <- helper_phf_pbp_data(plays_df) %>%
        make_fastRhockey_data("PHF Play-by-Play Information from PremierHockeyFederation.com",Sys.time())

      team_box_df <- data %>%
        helper_phf_team_box() %>%
        dplyr::mutate(game_id = game_id) %>%
        dplyr::select(
          .data$team, .data$game_id, .data$winner, .data$total_scoring, tidyr::everything()) %>%
        make_fastRhockey_data("PHF Team Box Information from PremierHockeyFederation.com",Sys.time())

      player_box_df <- phf_player_box(game_id = 420339)
      game_summary <- phf_game_summary(game_id = 268078)

      game <- c(
        list(plays_df),
        list(team_box_df),
        list(player_box_df$skaters),
        list(player_box_df$goalies),
        list(game_details),
        list(game_summary$scoring_summary),
        list(game_summary$shootout_summary),
        list(game_summary$penalty_summary),
        list(game_summary$officials),
        list(game_summary$team_staff),
        list(game_summary$timeouts))
      names(game) <- c(
        "plays",
        "team_box",
        "skaters",
        "goalies",
        "game_details",
        "scoring_summary",
        "shootout_summary",
        "penalty_summary",
        "officials",
        "team_staff",
        "timeouts")
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid game_id or no game data available!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(game)
}



#' @title **PHF Game Raw**
#' @description phf_game_raw: pull in the raw data for a game_id from the PHF/NWHL API
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @return A list of data frames
#' @import rvest
#' @import httr
#' @import jsonlite
#' @importFrom purrr pluck
#' @export
#' @examples
#' \donttest{
#'   try(phf_game_raw(game_id = 268078))
#' }
phf_game_raw <- function(game_id) {

  base_url <- "https://web.api.digitalshift.ca/partials/stats/game/play-by-play?game_id="
  full_url <- paste0(base_url, game_id)

  # setting the ticket as something that can be changed in case the API decides to change it's authorization
  # rather than hard-coding it in
  auth_ticket <- getOption(
    "fastRhockey.phf_ticket",
    default = 'ticket="4dM1QOOKk-PQTSZxW_zfXnOgbh80dOGK6eUb_MaSl7nUN0_k4LxLMvZyeaYGXQuLyWBOQhY8Q65k6_uwMu6oojuO"'
  )

  # the link for the game + authorization for accessing the API
  res <- httr::RETRY("GET", full_url,
                     httr::add_headers(`Authorization`= auth_ticket))
  # Check the result
  check_status(res)

  resp <- res %>%
    httr::content(as = "text", encoding="utf-8") %>%
    jsonlite::parse_json() %>%
    purrr::pluck("content") %>%
    rvest::read_html() %>%
    rvest::html_table()
  return(resp)
}


#' @title **PHF Game Details**
#' @description phf_game_details: pull in the raw data for a game_id from the PHF/NWHL API
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @return A data frame with game team details
#' @import rvest
#' @import httr
#' @import jsonlite
#' @importFrom purrr pluck
#' @export
#' @examples
#' \donttest{
#'   try(phf_game_details(game_id = 268078))
#' }
phf_game_details <- function(game_id) {

  base_url <- "https://web.api.digitalshift.ca/partials/stats/game?game_id="
  full_url <- paste0(base_url, game_id)

  # setting the ticket as something that can be changed in case the API decides to change it's authorization
  # rather than hard-coding it in
  auth_ticket <- getOption(
    "fastRhockey.phf_ticket",
    default = 'ticket="4dM1QOOKk-PQTSZxW_zfXnOgbh80dOGK6eUb_MaSl7nUN0_k4LxLMvZyeaYGXQuLyWBOQhY8Q65k6_uwMu6oojuO"'
  )

  # the link for the game + authorization for accessing the API
  res <- httr::RETRY("GET", full_url,
                     httr::add_headers(`Authorization`= auth_ticket))
  # Check the result
  check_status(res)

  resp <- (res %>%
    httr::content(as = "text", encoding="utf-8") %>%
    jsonlite::parse_json() %>%
    purrr::pluck("content") %>%
    rvest::read_html() %>%
    rvest::html_elements(".flex-row.flex-pcenter") %>%
    rvest::html_elements(".subtitle, .title, .title.center.score"))[5:10]
  away_location <- (resp %>%
                      rvest::html_text())[1]
  away_nickname <- (resp %>%
                      rvest::html_text())[2]
  away_abbreviation <- dplyr::case_when(
    away_location == "Boston" ~ "BOS",
    away_location == "Buffalo" ~ "BUF",
    away_location == "Connecticut" ~ "CTW",
    away_location == "Metropolitan" ~ "MET",
    away_location == "Montreal" ~ "MON",
    away_location == "Minnesota" ~ "MIN",
    away_location == "Toronto" ~ "TOR",
    TRUE ~ NA_character_)

  away_team <- paste(away_location, away_nickname)
  away_score <- (resp %>%
                      rvest::html_text())[3]
  home_location <- (resp %>%
                      rvest::html_text())[4]
  home_nickname <- (resp %>%
                      rvest::html_text())[5]
  home_abbreviation <- dplyr::case_when(
    home_location == "Boston" ~ "BOS",
    home_location == "Buffalo" ~ "BUF",
    home_location == "Connecticut" ~ "CTW",
    home_location == "Metropolitan" ~ "MET",
    home_location == "Montreal" ~ "MON",
    home_location == "Minnesota" ~ "MIN",
    home_location == "Toronto" ~ "TOR",
    TRUE ~ NA_character_)
  home_team <- paste(home_location, home_nickname)
  home_score <- (resp %>%
                   rvest::html_text())[6]
  game_details <- data.frame(
    "game_id" = game_id,
    "home_team" = home_team,
    "home_location" = home_location,
    "home_nickname" = home_nickname,
    "home_abbreviation" = home_abbreviation,
    "home_score_total" = as.integer(home_score),
    "away_team" = away_team,
    "away_location" = away_location,
    "away_nickname" = away_nickname,
    "away_abbreviation" = away_abbreviation,
    "away_score_total" = as.integer(away_score)
  )
  game_details <- game_details %>%
    make_fastRhockey_data("PHF Game Details Information from PremierHockeyFederation.com",Sys.time())
  return(game_details)
}


#' @title **PHF Game Summary**
#' @description phf_game_summary: pull in the raw data for a game_id from the PHF/NWHL API
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @return A named list of data frames: scoring_summary,shootout_summary, penalty_summary, officials, team_staff, timeouts
#' @import rvest
#' @import httr
#' @import jsonlite
#' @importFrom purrr pluck
#' @export
#' @examples
#' \donttest{
#'   try(phf_game_summary(game_id = 268078))
#' }
phf_game_summary <- function(game_id) {

  base_url <- "https://web.api.digitalshift.ca/partials/stats/game/boxscore?game_id="
  full_url <- paste0(base_url, game_id)

  # setting the ticket as something that can be changed in case the API decides to change it's authorization
  # rather than hard-coding it in
  auth_ticket <- getOption(
    "fastRhockey.phf_ticket",
    default = 'ticket="4dM1QOOKk-PQTSZxW_zfXnOgbh80dOGK6eUb_MaSl7nUN0_k4LxLMvZyeaYGXQuLyWBOQhY8Q65k6_uwMu6oojuO"'
  )

  # the link for the game + authorization for accessing the API
  res <- httr::RETRY("GET", full_url,
                     httr::add_headers(`Authorization`= auth_ticket))
  # Check the result
  check_status(res)

  resp <- res %>%
             httr::content(as = "text", encoding="utf-8") %>%
             jsonlite::parse_json() %>%
             purrr::pluck("content") %>%
             rvest::read_html() %>%
             rvest::html_table()

  scoring_summary <- resp[
    !sapply(
      lapply(resp, function(x){
        if("Scorer (Assists)" %in% colnames(x) && nrow(x)>0){
          return(x)
        }
      }),is.null)]

  if(length(scoring_summary)>0){
    scoring_summary <- scoring_summary[[1]]
    scoring_summary <- scoring_summary[1:6] %>%
      dplyr::filter(.data$Period %in% c("1st","2nd","3rd","OT1")) %>%
      janitor::clean_names()
  } else {
    scoring_summary <- data.frame()
  }

  shootout_summary <- resp[
    !sapply(
      lapply(resp, function(x){
        if("Scored" %in% colnames(x) && nrow(x)>0){
          return(x)
        }
      }),is.null)]

  if(length(shootout_summary)>0){
    shootout_summary <- shootout_summary[[1]] %>%
      janitor::clean_names()
  } else {
    shootout_summary <- data.frame()
  }

  penalty_summary <- resp[
    !sapply(
      lapply(resp, function(x){
        if("Infraction" %in% colnames(x) && nrow(x)>0){
          return(x)
        }
      }),is.null)]

  if(length(penalty_summary)>0){
    penalty_summary <- penalty_summary[[1]] %>%
      janitor::clean_names()
  } else {
    penalty_summary <- data.frame()
  }

  officials <- resp[
    !sapply(
      lapply(resp, function(x){
        if("Type" %in% colnames(x) && nrow(x)>0){
          return(x)
        }
      }),is.null)]

  if(length(officials)>0){
    officials <- officials[[1]] %>%
      janitor::clean_names()
  } else {
    officials <- data.frame()
  }

  team_staff <- resp[
    !sapply(
      lapply(resp, function(x){
        if("Team Staff" %in% colnames(x) && nrow(x)>0){
          return(x)
        }
      }),is.null)]
  if(length(team_staff)>0){
    team_staff <- team_staff[[1]]

    team_staff <- team_staff %>%
      dplyr::mutate(
        head_coach = stringr::str_extract(string = .data$`Team Staff`,"(.{0,25})(?<= - Head Coach)"),
        head_coach = stringr::str_remove(.data$head_coach," - Head Coach"),
        assistant_coach_1 = stringr::str_extract(string = .data$`Team Staff`,"- Head Coach,(.{0,35})(?<= - Assistant Coach)"),
        assistant_coach_1 = stringr::str_remove(.data$assistant_coach_1," - Assistant Coach"),
        assistant_coach_1 = stringr::str_remove(.data$assistant_coach_1,"- Head Coach, "),
        assistant_coach_2 = stringr::str_extract(string = .data$`Team Staff`," - Assistant Coach,(.{0,35})(?<= - Assistant Coach)$"),
        assistant_coach_2 = stringr::str_remove_all(.data$assistant_coach_2," - Assistant Coach"),
        assistant_coach_2 = stringr::str_remove(.data$assistant_coach_2,", ")) %>%
      dplyr::select(.data$Team, .data$head_coach, .data$assistant_coach_1,.data$assistant_coach_2) %>%
      dplyr::rename(team = .data$Team)
  } else {
    team_staff <- data.frame()
  }

  timeouts <- resp[[max(length(resp))]]
  if(!("Time" %in% colnames(timeouts))){
    timeouts <- data.frame()
  } else {
    timeouts <- timeouts %>%
      janitor::clean_names()
  }
  scoring_summary <- scoring_summary %>%
    make_fastRhockey_data("PHF Game Scoring Summary Information from PremierHockeyFederation.com",Sys.time())
  shootout_summary <- shootout_summary %>%
    make_fastRhockey_data("PHF Game Shootout Summary Information from PremierHockeyFederation.com",Sys.time())
  penalty_summary <- penalty_summary %>%
    make_fastRhockey_data("PHF Game Penalty Summary Information from PremierHockeyFederation.com",Sys.time())
  officials <- officials %>%
    make_fastRhockey_data("PHF Game Officials Information from PremierHockeyFederation.com",Sys.time())
  team_staff <- team_staff %>%
    make_fastRhockey_data("PHF Game Team Staff Information from PremierHockeyFederation.com",Sys.time())
  timeouts <- timeouts %>%
    make_fastRhockey_data("PHF Game Timeouts Information from PremierHockeyFederation.com",Sys.time())
  game_summary <- c(list(scoring_summary),
                    list(shootout_summary),
                    list(penalty_summary),
                    list(officials),
                    list(team_staff),
                    list(timeouts))
  names(game_summary) <- c("scoring_summary",
                           "shootout_summary",
                           "penalty_summary",
                           "officials",
                           "team_staff",
                           "timeouts")
  return(game_summary)
}

