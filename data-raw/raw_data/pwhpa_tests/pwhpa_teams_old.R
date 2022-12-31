
# phf_standings()

library(tidyverse)
library(rvest)
library(httr)

process_standings <- function(team_info) {
  
  rank <- html_text(team_info[[1]])
  team_name <- html_text(team_info[[2]])
  
  link <- team_info[[2]] %>% html_node("a") %>% html_attr("href")
  
  team_link <- stringr::str_remove(stringr::str_remove(link, "https://stats.pwhpa.com/team/"), "/")
  
  gp <- html_text(team_info[[3]])
  w <- html_text(team_info[[4]])
  l <- html_text(team_info[[5]])
  otw <- html_text(team_info[[6]])
  otl <- html_text(team_info[[7]])
  pts <- html_text(team_info[[8]])
  gf <- html_text(team_info[[9]])
  ga <- html_text(team_info[[10]])
  plus_minus <- html_text(team_info[[11]])
  # scales::percent(as.numeric(pppct), scale = 1)
  pppct <- as.numeric(html_text(team_info[[12]]))
  pk <- as.numeric(html_text(team_info[[13]]))
  lfive <- html_text(team_info[[14]])
  strk <- html_text(team_info[[15]])
  
  team_df <- data.frame(
    season = c(2022),
    rank = c(rank),
    team = c(team_name),
    team_id = c(team_link),
    gp = c(gp),
    w = c(w),
    l = c(l),
    otw = c(otw),
    otl = c(otl),
    pts = c(pts),
    gf = c(gf),
    ga = c(ga),
    diff = c(plus_minus),
    power_play_pct = c(pppct),
    penalty_kill_pct = c(pk),
    last_five = c(lfive),
    streak = c(strk)
  )
  
  return(team_df)
  
}

pwhpa_standings <- function() {
  
  full_url <- "https://stats.pwhpa.com/table/sdgt-standings/"
  
  # the link for the game + authorization for accessing the API
  res <- httr::RETRY("GET", full_url)
  
  lst <- res %>%
    httr::content(as = "text", encoding="utf-8") %>%
    # purrr::pluck("content") %>%
    rvest::read_html() %>%
    rvest::html_elements("body")
  
  tms <- lst[1] %>%
    rvest::html_nodes("tr")
  
  lst <- list()
  
  for (id in seq(1:length(tms))) {
    
    team_info <- tms[[id]] %>%
      html_nodes("td")
    
    team_df <- process_standings(team_info = team_info)
    
    lst[[id]] <- team_df
    
  }
  
  standings <- dplyr::bind_rows(lst) %>%
    tibble()
  
  return(standings)
  
}


