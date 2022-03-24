#' @title phf_player_stats
#' @description phf_player_stats: loads the player stats
#'
#' @param player_id The unique ID code for the player that you are interested in viewing the data for
#' @return A named list of data frames: career, game_log
#' @import rvest
#' @import janitor
#' @import httr
#' @import stringr
#' @import jsonlite
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(phf_player_stats(player_id = 431611))
#' }
phf_player_stats <- function(player_id) {

  tryCatch(
    expr = {

      y <- player_id

      base_url <- "https://web.api.digitalshift.ca/partials/stats/player?player_id="
      full_url <- paste0(base_url, player_id)

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

      resp_all <- res %>%
        httr::content(as = "text", encoding="utf-8") %>%
        jsonlite::parse_json() %>%
        purrr::pluck("content") %>%
        rvest::read_html()

      resp <- resp_all %>%
        rvest::html_elements("table")

      resp <- unique(resp)
      player_name <- resp_all %>%
        rvest::html_elements(".title") %>%
        rvest::html_text()
      position <- resp_all %>%
        rvest::html_elements(".subtitle") %>%
        rvest::html_text()
      position <- stringr::str_remove(position,"#\\d+ ")
      regular_season_href <- resp[[1]] %>%
        rvest::html_elements("tr") %>%
        rvest::html_elements("td > a") %>%
        rvest::html_attr("href")

      playoffs_href <- resp[[3]] %>%
        rvest::html_elements("tr") %>%
        rvest::html_elements("td > a") %>%
        rvest::html_attr("href")

      player_game_log_href <- resp[[5]] %>%
        rvest::html_elements("tr") %>%
        rvest::html_elements("td > a") %>%
        rvest::html_attr("href")

      regular_season_href <- append(regular_season_href,NA_character_)
      playoffs_href <- append(playoffs_href,NA_character_)
      player_game_log_href <- player_game_log_href

      regular_season_stats <- resp[[1]] %>%
        rvest::html_table()
      playoff_stats <- resp[[3]] %>%
        rvest::html_table()
      player_game_log_stats <- resp[[5]] %>%
        rvest::html_table()

      regular_season_href <- data.frame(
        player_name = player_name,
        player_id = player_id,
        position = position,
        team_href = regular_season_href,
        season_type = "Regular Season")
      playoffs_href <- data.frame(
        player_name = player_name,
        player_id = player_id,
        position = position,
        team_href = playoffs_href,
        season_type = "Playoffs")
      player_game_log_href <- data.frame(
        player_name = player_name,
        player_id = player_id,
        position = position,
        game_href = player_game_log_href)

      regular_season_stats <- dplyr::bind_cols(regular_season_stats, regular_season_href)
      playoff_stats <- dplyr::bind_cols(playoff_stats, playoffs_href)
      game_log_stats <- dplyr::bind_cols(player_game_log_stats, player_game_log_href)


      yearly_stats <- dplyr::bind_rows(regular_season_stats, playoff_stats)

      if(position != "Goalie"){
        suppressWarnings(
          yearly_stats <- yearly_stats %>%
            tidyr::separate(.data$`FoW/L`,into = c("faceoffs_won", "faceoffs_lost"),
                            sep = " - ", remove = FALSE) %>%
            dplyr::mutate_at(c("faceoffs_won","faceoffs_lost"), function(x){
              as.integer(x)
            })
        )
        suppressWarnings(
          game_log_stats <- game_log_stats %>%
            tidyr::separate(.data$`FoW/L`,into = c("faceoffs_won", "faceoffs_lost"),
                            sep = " - ", remove = FALSE) %>%
            dplyr::mutate_at(c("faceoffs_won","faceoffs_lost"), function(x){
              as.integer(x)
            })
        )
      yearly_stats <- yearly_stats %>%
        dplyr::rename(
          season = .data$Season,
          team_name = .data$Team,
          division = .data$Division,
          games_played = .data$GP,
          goals = .data$G,
          assists = .data$A,
          points = .data$Pts,
          points_per_game_average = .data$PPGA,
          penalty_minutes = .data$PIM,
          plus_minus = .data$`+/-`,
          shots_on_goal = .data$SOG,
          scoring_pct = .data$`S%`,
          blocks = .data$Blk,
          giveaways = .data$GvA,
          takeaways = .data$TkA,
          faceoffs_won_lost = .data$`FoW/L`,
          faceoffs_win_pct = .data$`Fo%`,
          powerplay_goals = .data$`PPG`,
          shorthanded_goals = .data$`SHG`,
          game_winning_goals = .data$`GWG`,
          shots = .data$`Sh`,
          shots_blocked = .data$`ShBl`) %>%
        dplyr::mutate(
          player_id = player_id,
          team_id = as.integer(stringr::str_extract(stringr::str_remove(.data$team_href,"stats#/100/team"), "\\d+"))) %>%
        dplyr::select(-.data$Pos) %>%
        make_fastRhockey_data("PHF Skaters Yearly Stats Information from PremierHockeyFederation.com",Sys.time())
      game_log_stats <- game_log_stats %>%
        dplyr::rename(
          date = .data$Date,
          opponent = .data$Opponent,
          result = .data$Result,
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
          shots = .data$`Sh`,
          shots_blocked = .data$`ShBl`) %>%
        dplyr::mutate(
          game_id = as.integer(stringr::str_extract(stringr::str_remove(.data$game_href,"stats#/100/game"), "\\d+"))) %>%
        make_fastRhockey_data("PHF Skaters Game Log Stats Information from PremierHockeyFederation.com",Sys.time())

      }else{
      yearly_stats <- yearly_stats %>%
        dplyr::rename(
          season = .data$Season,
          team_name = .data$Team,
          division = .data$Division,
          shots_against = .data$SA,
          goals_against = .data$GA,
          saves = .data$Sv,
          save_percent = .data$`Sv%`,
          minutes_played = .data$MP,
          penalty_minutes = .data$PIM,
          goals = .data$G,
          assists = .data$A) %>%
        dplyr::mutate(
          team_id = as.integer(stringr::str_extract(stringr::str_remove(.data$team_href,"stats#/100/team"), "\\d+"))) %>%
        make_fastRhockey_data("PHF Goalies Yearly Stats Information from PremierHockeyFederation.com",Sys.time())
      game_log_stats <- game_log_stats %>%
        dplyr::rename(
          date = .data$Date,
          opponent = .data$Opponent,
          result = .data$Result,
          shots_against = .data$SA,
          goals_against = .data$GA,
          saves = .data$Sv,
          save_percent = .data$`Sv%`,
          minutes_played = .data$MP,
          penalty_minutes = .data$PIM,
          goals = .data$G,
          assists = .data$A) %>%
        dplyr::mutate(
          game_id = as.integer(stringr::str_extract(stringr::str_remove(.data$game_href,"stats#/100/game"), "\\d+"))) %>%
        make_fastRhockey_data("PHF Goalies Game Log Stats Information from PremierHockeyFederation.com",Sys.time())

      }
      player_stats <- c(list(yearly_stats),list(game_log_stats))
      names(player_stats) <- c("career","game_log")

    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid game_id or no player box data available! Try using `phf_schedule` to find a game ID to look up!"))
    },
    warning = function(w) {

    },
    finally = {

    }
  )

  return(player_stats)

}
