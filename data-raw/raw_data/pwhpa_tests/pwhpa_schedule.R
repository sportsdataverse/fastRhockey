library(tidyverse)
library(rvest)
library(httr)

process_game_info <- function(game_info) {

  date <- substr(html_text(game_info[[1]]), 0, 19)
  date_text <- substr(html_text(game_info[[1]]), 20, nchar(html_text(game_info[[1]])))

  home_tm <- stringr::str_trim(html_text(game_info[[2]]))
  away_tm <- stringr::str_trim(html_text(game_info[[4]]))

  if (grepl("pm|am", html_text(game_info[[3]]))) {

    home_scr <- NA
    away_scr <- NA

  } else {

    home_scr <- stringr::str_split(html_text(game_info[[3]]), " - ", n = 2)[[1]][1]
    away_scr <- stringr::str_split(html_text(game_info[[3]]), " - ", n = 2)[[1]][2]

  }

  location <- html_text(game_info[[5]])

  game_link <- game_info[[6]] %>% html_node("a") %>% html_attr("href")

  game_id <- stringr::str_remove(stringr::str_remove(game_link, "https://stats.pwhpa.com/event/"), '/')

  winner <- ifelse(home_scr > away_scr, home_tm, ifelse(away_scr > home_scr, away_tm, "Tie"))

  game_df <- data.frame(
    game_date = c(date),
    game_id = c(game_id),
    date_text = c(date_text),
    home_team = c(home_tm),
    home_score = c(home_scr),
    away_team = c(away_tm),
    away_score = c(away_scr),
    winner = c(winner),
    location = c(location),
    game_link = c(game_link)
  )

  return(game_df)

}

#' @title  **PWHPA Schedule**
#' @description PWHPA Schedule lookup
#'
#' @return A data frame with schedule data
#' @import rvest
#' @import dplyr
#' @import httr
#' @export
#' @examples
#' \donttest{
#'   try(pwhpa_schedule(season=2022))
#' }
pwhpa_schedule <- function() {

  full_url <- "https://stats.pwhpa.com/calendar/secret-dream-gap-tour-schedule/"

  # the link for the game + authorization for accessing the API
  res <- httr::RETRY("GET", full_url)

  lst <- res %>%
    httr::content(as = "text", encoding="utf-8") %>%
    # purrr::pluck("content") %>%
    rvest::read_html() %>%
    rvest::html_elements("tbody")

  g <- lst[1] %>%
    rvest::html_nodes("tr")

  lst <- list()

  for (id in seq(1:length(g))) {

    game_info <- g[[id]] %>%
      rvest::html_nodes("td")

    game_df <- process_game_info(game_info = game_info)

    lst[[id]] <- game_df

  }

  schedule_df <- dplyr::bind_rows(lst)

  return(schedule_df)

}

# df <- pwhpa_schedule()
