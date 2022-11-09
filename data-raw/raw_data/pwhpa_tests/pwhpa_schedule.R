library(tidyverse)
library(rvest)
library(httr)

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

  n <- g[[1]] %>%
    rvest::html_nodes("td")

  date <- substr(html_text(n[[1]]), 0, 19)
  date_text <- substr(html_text(n[[1]]), 20, nchar(html_text(n[[1]])))

  home_tm <- stringr::str_trim(html_text(n[[2]]))
  away_tm <- stringr::str_trim(html_text(n[[4]]))
  home_scr <- stringr::str_split(html_text(n[[3]]), " - ", n = 2)[[1]][1]
  away_scr <- stringr::str_split(html_text(n[[3]]), " - ", n = 2)[[1]][2]

  location <- html_text(n[[5]])

  game_link <- n[[6]] %>% html_node("a") %>% html_attr("href")

  game_id <- stringr::str_remove(stringr::str_remove(game_link, "https://stats.pwhpa.com/event/"), '/')

  winner <- ifelse(home_scr > away_scr, home_tm, ifelse(away_scr > home_scr, away_tm, "Tie"))

  data.frame(
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

  # print.AsIs(g[1])

  # for (y in g[[1]]) {
  #
  #   print(y)
  #
  # }

}
