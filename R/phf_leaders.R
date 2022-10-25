#' @title **PHF Player Leaderboards**
#' @description PHF Player Leaderboards
#' @param player_type Player type: skaters, goalies
#' @param season Season (YYYY) to pull the team stats from, the concluding year in XXXX-YY format
#' @param season_type Season type: Regular Season or Playoffs
#' @return A data frame of stat leaders
#' @import rvest
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'  try(phf_leaders(player_type = "skaters", season = 2022, season_type="Regular Season"))
#'  try(phf_leaders(player_type = "goalies", season = 2022, season_type="Regular Season"))
#' }

phf_leaders <- function(player_type, season = 2021, season_type="Regular Season"){

  league_info <- phf_league_info(season=season)
  season_id <- phf_get_season_id(season=season)
  team_row <- league_info$teams

  player_type <- ifelse(player_type == "skaters", "players", player_type)
  season_type <- gsub(" ","+", season_type)
  base_url <- "https://web.api.digitalshift.ca/partials/stats/leaders/table?player_type="
  full_url <- paste0(base_url,
                     player_type,
                     "&division_id=", unique(team_row$division_id),
                     "&game_type=", season_type,
                     "&season_id=", season_id,
                     "&limit=350&all=true"
  )

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
      skaters_href <- resp[[1]] %>%
        rvest::html_elements("tr") %>%
        rvest::html_elements("td > a.person-inline") %>%
        rvest::html_attr("href")
      team_href <- resp[[1]] %>%
        rvest::html_elements("tr") %>%
        rvest::html_elements("td > a.team-inline") %>%
        rvest::html_attr("href")

      skaters <- resp[[1]] %>%
        rvest::html_table()

      skaters_href <- data.frame(
        season_type = season_type,
        player_href = skaters_href,
        team_href = team_href
      )

      skaters <- dplyr::bind_cols(skaters, skaters_href)


      if(player_type=="players"){
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
            "player_jersey" = "#",
            "player_name" = "Name",
            "team_name" = "Team",
            "position" = "Pos",
            "games_played" = "GP",
            "goals" = "G",
            "assists" = "A",
            "points" = "Pts",
            "points_per_game_average" = "PPGA",
            "penalty_minutes" = "PIM",
            "plus_minus" = "+/-",
            "shots_on_goal" = "SOG",
            "scoring_pct" = "S%",
            "blocks" = "Blk",
            "giveaways" = "GvA",
            "takeaways" = "TkA",
            "faceoffs_won_lost" = "FoW/L",
            "faceoffs_win_pct" = "Fo%",
            "powerplay_goals" = "PPG",
            "shorthanded_goals" = "SHG",
            "game_winning_goals" = "GWG",
            "shots" = "Sh",
            "shots_blocked" = "ShBl") %>%
          dplyr::mutate(
            player_name = stringr::str_replace(.data$player_name, pattern = "#\\d+",replacement=""),
            player_id = as.integer(stringr::str_extract(.data$player_href, "\\d+")),
            team_id = as.integer(stringr::str_extract(stringr::str_remove(.data$team_href,"stats#/100/team"), "\\d+"))

          )
      }else{

        skaters <- skaters %>%
          dplyr::rename(
            "player_jersey" = "#",
            "player_name" = "Name",
            "team_name" = "Team",
            "games_played" = "GP",
            "wins" = "W",
            "losses" = "L",
            "ties" = "T",
            "overtime_losses" = "OTL",
            "shots_against" = "SA",
            "goals_against" = "GA",
            "saves" = "Sv",
            "save_percent" = "Sv%",
            "goals_against_average" = "GAA",
            "shutouts" = "SO",
            "minutes_played" = "MP",
            "penalty_minutes" = "PIM",
            "goals" = "G",
            "assists" = "A") %>%
          dplyr::mutate(
            player_name = stringr::str_replace(.data$player_name,pattern = "#\\d+",replacement=""),
            player_id = as.integer(stringr::str_extract(.data$player_href, "\\d+")),
            team_id = as.integer(stringr::str_extract(stringr::str_remove(.data$team_href,"stats#/100/team"), "\\d+"))

          )

      }
      skaters <- skaters %>%
        make_fastRhockey_data("PHF Leaderboard Information from PremierHockeyFederation.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid parameters or no leaderboard data available!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(skaters)
}
