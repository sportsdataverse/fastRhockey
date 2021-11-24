#' @title phf_player_box
#' @description phf_player_box: loads the player boxscore
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @import rvest
#' @import janitor
#' @import httr
#' @import stringr
#' @import jsonlite
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   phf_player_box(game_id = 420339)
#' }
phf_player_box <- function(game_id = 420339) {

  tryCatch(
    expr = {

      y <- game_id

      base_url <- "https://web.api.digitalshift.ca/partials/stats/game/team-stats?game_id="
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

      resp <- unique(resp)
      away_skaters <- resp[[1]]
      away_goalies <- resp[[2]]
      home_skaters <- resp[[3]]
      home_goalies <- resp[[4]]

      skaters <- dplyr::bind_rows(home_skaters, away_skaters)
      goalies <- dplyr::bind_rows(home_goalies, away_goalies)

      skaters <- skaters %>%
        dplyr::rename(
          player_number = .data$`#`,
          player_name = .data$Name,
          position = .data$Pos,
          goals = .data$G,
          assists = .data$A,
          points = .data$Pts,
          penalty_minutes = .data$PIM,
          plus_minus = .data$`+/-`,
          shots_on_goal = .data$SOG,
          blocks = .data$Blk,
          giveaways = .data$GvA,
          takeaways = .data$TkA,
          faceoffs_won_lost = .data$`FoW/L`,
          faceoffs_win_pct = .data$`Fo%`,
          powerplay_goals = .data$`PPG`,
          shorthanded_goals = .data$`SHG`,
          shots = .data$`Sh`,
          shots_blocked = .data$`ShBl`,
          faceoffs_won = .data$`FoW`,
          faceoffs_lost = .data$`FoL`) %>%
        dplyr::mutate(
          player_name = stringr::str_replace(.data$player_name,pattern = "#\\d+",replacement="")
        )
      goalies <- goalies %>%
        dplyr::rename(
          player_number = .data$`#`,
          player_name = .data$Name,
          shots_against = .data$SA,
          goals_against = .data$GA,
          saves = .data$Sv,
          save_percent = .data$`Sv%`,
          minutes_played = .data$MP,
          penalty_minutes = .data$PIM,
          goals = .data$G,
          assists = .data$A) %>%
        dplyr::mutate(
          player_name = stringr::str_replace(.data$player_name,pattern = "#\\d+",replacement="")
        )
      player_box <- c(list(skaters), list(goalies))
      names(player_box) <- c("skaters", "goalies")
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid game_id or no player box data available! Try using `phf_schedule` to find a game ID to look up!"))
    },
    warning = function(w) {

    },
    finally = {

    }
  )

  return(player_box)

}
