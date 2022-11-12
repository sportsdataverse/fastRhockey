
# PWHPA JSON Lists
# Calenders
# Tables
# Events
# Teams
# Players
# Staff
# Leagues
# Seasons
# Venues
# Positions
# Roles

library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(rjson)
library(ggimage)
library(ggtext)

get_pwhpa_player_headshots <- function() {
  
}

pwhpa_something <- function() {
  
  base_url <- "https://stats.pwhpa.com/wp-json/sportspress/v2/"
  full_url <- paste0(base_url, "lists")
  
  all_lists <- httr::GET(full_url) %>%
    httr::content(as = "text") %>%
    fromJSON()
    # jsonlite::parse_json()
  
  data_list <- all_lists[[2]]$data
  
  process_data_list <- function(data_list) {
    tibble::tibble(
      player_id = names(data_list),
      data = purrr::map(data_list, dplyr::bind_rows)
    ) %>%
      tidyr::unnest_wider(data)
  }
  
  process_data_list2 <- function(data_list) {
    suppressWarnings(data.table::rbindlist(data_list,use.names = TRUE,fill = TRUE,idcol = "player_id"))
  }
  
  df_list <- tibble::tibble(all_lists) %>%
    tidyr::unnest_wider(1) %>%
    dplyr::select(title, data, link) %>%
    dplyr::mutate(
      title = unlist(title) %>% unname(),
      data = purrr::map(data, process_data_list)
    )
  
  df_list <- tibble::tibble(all_lists) %>%
    tidyr::unnest_wider(1) %>%
    dplyr::select(title, data, link) %>%
    dplyr::mutate(
      title = unlist(title) %>% unname(),
      data = purrr::map(data, process_data_list2)
    )
  
  # tm <- 
    df_list %>%
    dplyr::filter(title == "Montreal Showcase 2022") %>%
    tidyr::unnest(data)
  
  # stats <- 
    df_list %>%
    dplyr::filter(title == "PWHPA Roster") %>%
    tidyr::unnest(data) %>%
    dplyr::select(
      name, player_id, team, position, sc,
      dob, age, number,
      eventsplayed, eventsstarted
      # title, player_id, name, team, position, link, gp, g, a, toi, pim, 
      # eventsplayed, eventsstarted, 
      # gw, ga, sa, sv, so, 
      # overtimeloss, win, loss, otwin,
      # sc, hometown, collegeuniversity,
      # p, gaa, svs, svp, ttoi, dob, age, number
    )
  
  # base_url <- "https://stats.pwhpa.com/wp-json/sportspress/v2/"
  # full_url <- paste0(base_url,
  #                    "lists")
  # 
  # res <- httr::RETRY("GET", full_url)
  # 
  # data <- res %>%
  #   httr::content(as = "text", encoding="utf-8") %>%
  #   fromJSON()
  # 
  # names(data[[2]]$data)
  # 
  # for (id in names(data[[2]]$data)) {
  #   
  #   # print(as.name(id))
  #   # data[[2]]$data$`0`
  #   data[[2]]$data$as.name(id)
  # }
  # 
  # length(data[[2]]$data$`0`)
  # 
  # data.frame(unlist(data[[2]]$data$`163`))
  # do.call(rbind.data.frame, data[[2]]$data$`163`)
  # 
  # do.call(rbind.data.frame, data[[2]]$data)

}

