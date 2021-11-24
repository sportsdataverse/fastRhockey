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
