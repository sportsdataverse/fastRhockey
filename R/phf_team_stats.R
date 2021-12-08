#' @title phf_team_stats
#' @description PHF Team Stats lookup
#' @param team Team name with nickname (e.g. Boston Pride, Buffalo Beauts)
#' @param season Season (YYYY) to pull the team stats from, the concluding year in XXXX-YY format
#' @return A named list of data frames: skaters, goalies
#' @import rvest
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'  phf_team_stats(team = "Boston Pride", season = 2022)
#' }

phf_team_stats <- function(team, season = 2022){

  league_info <- phf_league_info(season=season)
  season_id <- dplyr::case_when(
    season == 2022 ~ 3372,
    season == 2021 ~ 2779,
    season == 2020 ~ 1950,
    season == 2019 ~ 2047,
    season == 2018 ~ 2046,
    season == 2017 ~ 2045,
    season == 2016 ~ 246,
    TRUE ~ NA_real_
  )
  team_row <- league_info$teams %>%
    dplyr::filter(.data$name == team)
  team_id <- team_row %>%
    dplyr::select(.data$id)
  base_url <- "https://web.api.digitalshift.ca/partials/stats/team/stats?team_id="
  full_url <- paste0(base_url,
                     team_id$id)

  # the link for the game + authorization for accessing the API
  res <- httr::RETRY(
    "GET", full_url,
    httr::add_headers(
      `Authorization`='ticket="4dM1QOOKk-PQTSZxW_zfXnOgbh80dOGK6eUb_MaSl7nUN0_k4LxLMvZyeaYGXQuLyWBOQhY8Q65k6_uwMu6oojuO"'))

  # Check the result
  check_status(res)

  tryCatch(
    expr={
      resp <- (res %>%
                 httr::content(as = "text", encoding="utf-8") %>%
                 jsonlite::parse_json() %>%
                 purrr::pluck("content") %>%
                 rvest::read_html() %>%
                 rvest::html_elements("table"))

      resp <- unique(resp)
      regular_season_skaters_href <- resp[[1]] %>%
        rvest::html_elements("tr") %>%
        rvest::html_elements("td > a") %>%
        rvest::html_attr("href")

      regular_season_goalies_href <- resp[[3]] %>%
        rvest::html_elements("tr") %>%
        rvest::html_elements("td > a") %>%
        rvest::html_attr("href")

      regular_season_skaters <- resp[[1]] %>%
        rvest::html_table()
      regular_season_goalies <- resp[[3]] %>%
        rvest::html_table()

      regular_season_skaters_href <- data.frame(
        season_type = "Regular Season",
        skaters_href = regular_season_skaters_href
      )
      regular_season_goalies_href <- data.frame(
        season_type = "Regular Season",
        goalies_href = regular_season_goalies_href
      )
      regular_season_skaters <- dplyr::bind_cols(regular_season_skaters, regular_season_skaters_href)
      regular_season_goalies <- dplyr::bind_cols(regular_season_goalies, regular_season_goalies_href)
      if(length(resp)>4){
        playoff_skaters_href <- resp[[5]] %>%
          rvest::html_elements("tr") %>%
          rvest::html_elements("td > a") %>%
          rvest::html_attr("href")

        playoff_goalies_href <- resp[[7]] %>%
          rvest::html_elements("tr") %>%
          rvest::html_elements("td > a") %>%
          rvest::html_attr("href")

        playoff_skaters <- resp[[5]] %>%
          rvest::html_table()
        playoff_goalies <- resp[[7]] %>%
          rvest::html_table()

        playoff_skaters_href <- data.frame(
          season_type = "Playoffs",
          skaters_href = playoff_skaters_href
        )
        playoff_goalies_href <- data.frame(
          season_type = "Playoffs",
          goalies_href = playoff_goalies_href
        )
        playoff_skaters <- dplyr::bind_cols(playoff_skaters, playoff_skaters_href)
        playoff_goalies <- dplyr::bind_cols(playoff_goalies, playoff_goalies_href)

        skaters <- dplyr::bind_rows(regular_season_skaters, playoff_skaters)
        goalies <- dplyr::bind_rows(regular_season_goalies, playoff_goalies)
      }else{
        skaters <- regular_season_skaters
        goalies <- regular_season_goalies
      }
      skaters <- dplyr::bind_cols(team_row, skaters)
      suppressWarnings(
        skaters <- skaters %>%
          tidyr::separate(.data$`FoW/L`,into = c("faceoffs_won", "faceoffs_lost"),
                          sep = " - ", remove = FALSE) %>%
          dplyr::mutate_at(c("faceoffs_won","faceoffs_lost"), function(x){
            as.integer(x)
          })
      )
      skaters <- skaters %>%
        dplyr::rename(
          team_id = .data$id,
          team_name = .data$name,
          player_jersey = .data$`#`,
          player_name = .data$Name,
          position = .data$Pos,
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
          player_name = stringr::str_replace(.data$player_name,pattern = "#\\d+",replacement=""),
          player_id = as.integer(stringr::str_extract(.data$skaters_href, "\\d+"))
        )

      goalies <- dplyr::bind_cols(team_row, goalies)
      goalies <- goalies %>%
        dplyr::rename(
          team_id = .data$id,
          team_name = .data$name,
          player_jersey = .data$`#`,
          player_name = .data$Name,
          games_played = .data$GP,
          wins = .data$W,
          losses = .data$L,
          ties = .data$T,
          overtime_losses = .data$OTL,
          shots_against = .data$SA,
          goals_against = .data$GA,
          saves = .data$Sv,
          save_percent = .data$`Sv%`,
          goals_against_average = .data$GAA,
          shutouts = .data$SO,
          minutes_played = .data$MP,
          penalty_minutes = .data$PIM,
          goals = .data$G,
          assists = .data$A,
          games_started = .data$GS) %>%
        dplyr::mutate(
          player_name = stringr::str_replace(.data$player_name,pattern = "#\\d+",replacement=""),
          player_id = as.integer(stringr::str_extract(.data$goalies_href, "\\d+"))
        )
      team_stats <- c(list(skaters), list(goalies))
      names(team_stats) <- c("skaters", "goalies")


    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid parameters or no team stats data available!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(team_stats)
}
