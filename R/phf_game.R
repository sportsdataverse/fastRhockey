#' @title phf_game_all
#' @description phf_game_all: pull in the raw data for a game_id from the PHF/NWHL API
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @import rvest
#' @import httr
#' @import dplyr
#' @importFrom jsonlite parse_json
#' @importFrom purrr pluck map_dfr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   phf_game_all(game_id = 268127)
#' }
phf_game_all <- function(game_id = 368719) {
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
      team_box_df <- data %>%
        helper_phf_team_box() %>%
        dplyr::mutate(game_id = game_id) %>%
        dplyr::select(
          .data$team, .data$game_id, .data$winner, .data$total_scoring, tidyr::everything())


      game <- c(plays_df, team_box_df)
      names(game) <- c("plays", "team_box")
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



#' @title phf_game_raw
#' @description phf_game_raw: pull in the raw data for a game_id from the PHF/NWHL API
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @import rvest
#' @import httr
#' @import jsonlite
#' @importFrom purrr pluck
#' @export
#' @examples
#' \donttest{
#'   phf_game_raw(game_id = 268078)
#' }
phf_game_raw <- function(game_id = 268078) {

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


#' @title phf_game_details
#' @description phf_game_details: pull in the raw data for a game_id from the PHF/NWHL API
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @import rvest
#' @import httr
#' @import jsonlite
#' @importFrom purrr pluck
#' @export
#' @examples
#' \donttest{
#'   phf_game_details(game_id = 268078)
#' }
phf_game_details <- function(game_id = 268078) {

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
  away_team <- glue::glue("{away_location} {away_nickname}")
  away_score <- (resp %>%
                      rvest::html_text())[3]
  home_location <- (resp %>%
                      rvest::html_text())[4]
  home_nickname <- (resp %>%
                      rvest::html_text())[5]
  home_team <- glue::glue("{home_location} {home_nickname}")
  home_score <- (resp %>%
                   rvest::html_text())[6]
  game_details <- data.frame(
    "game_id" = game_id,
    "home_team" = home_team, "home_location" = home_location, "home_nickname" = home_nickname,
    "home_score_total" = as.integer(home_score),
    "away_team" = away_team, "away_location" = away_location, "away_nickname" = away_nickname,
    "away_score_total" = as.integer(away_score)
  )
  return(game_details)
}


#' @title phf_game_summary
#' @description phf_game_summary: pull in the raw data for a game_id from the PHF/NWHL API
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @import rvest
#' @import httr
#' @import jsonlite
#' @importFrom purrr pluck
#' @export
#' @examples
#' \donttest{
#'   phf_game_summary(game_id = 268078)
#' }
phf_game_summary <- function(game_id = 268127) {

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

