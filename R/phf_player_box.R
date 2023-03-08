#' @title **PHF Player Boxscore**
#' @description phf_player_box: loads the player boxscore
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @return A named list of data frames: skaters, goalies
#' @import rvest
#' @import janitor
#' @import httr
#' @import stringr
#' @import jsonlite
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(phf_player_box(game_id = 420339))
#' }
phf_player_box <- function(game_id) {

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
      game_details <- phf_game_details(game_id)
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
        rvest::html_elements("table"))

      resp <- unique(resp)
      away_skaters_href <- resp[[1]] %>%
        rvest::html_elements("tr") %>%
        rvest::html_elements("td > a") %>%
        rvest::html_attr("href")

      away_goalies_href <- resp[[3]] %>%
        rvest::html_elements("tr") %>%
        rvest::html_elements("td > a") %>%
        rvest::html_attr("href")

      home_skaters_href <- resp[[5]] %>%
        rvest::html_elements("tr") %>%
        rvest::html_elements("td > a") %>%
        rvest::html_attr("href")

      home_goalies_href <- resp[[7]] %>%
        rvest::html_elements("tr") %>%
        rvest::html_elements("td > a") %>%
        rvest::html_attr("href")

      away_skaters <- resp[[1]] %>%
        rvest::html_table()
      away_goalies <- resp[[3]] %>%
        rvest::html_table()
      home_skaters <- resp[[5]] %>%
        rvest::html_table()
      home_goalies <- resp[[7]] %>%
        rvest::html_table()
      away_skaters$team <- game_details$away_team
      away_goalies$team <- game_details$away_team
      home_skaters$team <- game_details$home_team
      home_goalies$team <- game_details$home_team
      away_skaters_href <- data.frame(skaters_href = away_skaters_href)
      home_skaters_href <- data.frame(skaters_href = home_skaters_href)
      away_goalies_href <- data.frame(goalies_href = away_goalies_href)
      home_goalies_href <- data.frame(goalies_href = home_goalies_href)
      home_skaters <- dplyr::bind_cols(home_skaters, home_skaters_href)
      away_skaters <- dplyr::bind_cols(away_skaters, away_skaters_href)
      home_goalies <- dplyr::bind_cols(home_goalies, home_goalies_href)
      away_goalies <- dplyr::bind_cols(away_goalies, away_goalies_href)

      skaters <- dplyr::bind_rows(home_skaters, away_skaters)
      goalies <- dplyr::bind_rows(home_goalies, away_goalies)

      skaters <- skaters %>%
        dplyr::rename(dplyr::any_of(c(
          "player_jersey" = "#",
          "player_name" = "Name",
          "position" = "Pos",
          "goals" = "G",
          "assists" = "A",
          "points" = "Pts",
          "penalty_minutes" = "PIM",
          "plus_minus" = "+/-",
          "shots_on_goal" = "SOG",
          "blocks" = "Blk",
          "giveaways" = "GvA",
          "takeaways" = "TkA",
          "faceoffs_won_lost" = "FoW/L",
          "faceoffs_win_pct" = "Fo%",
          "powerplay_goals" = "PPG",
          "shorthanded_goals" = "SHG",
          "shots" = "Sh",
          "shots_blocked" = "ShBl",
          "faceoffs_won" = "FoW",
          "faceoffs_lost" = "FoL"))) %>%
        dplyr::mutate(
          player_name = stringr::str_replace(.data$player_name,pattern = "#\\d+",replacement=""),
          player_id = stringr::str_extract(.data$skaters_href, "\\d+"),
          game_id = y
        ) %>%
        make_fastRhockey_data("PHF Skaters Boxscore Information from PremierHockeyFederation.com",Sys.time())
      goalies <- goalies %>%
        dplyr::rename(dplyr::any_of(c(
          "player_jersey" = "#",
          "player_name" = "Name",
          "shots_against" = "SA",
          "goals_against" = "GA",
          "saves" = "Sv",
          "save_percent" = "Sv%",
          "minutes_played" = "MP",
          "penalty_minutes" = "PIM",
          "goals" = "G",
          "assists" = "A"))) %>%
        dplyr::mutate(
          player_name = stringr::str_replace(.data$player_name,pattern = "#\\d+",replacement=""),
          player_id = stringr::str_extract(.data$goalies_href, "\\d+"),
          game_id = y
        ) %>%
        make_fastRhockey_data("PHF Goalies Boxscore Information from PremierHockeyFederation.com",Sys.time())
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
