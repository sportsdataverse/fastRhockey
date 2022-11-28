library(tidyverse)
# library(rvest)
# library(httr)
# library(jsonlite)
# library(rjson)
# library(ggimage)
# library(ggtext)

pwhpa_standings <- function() {

  base_url <- "https://stats.pwhpa.com/wp-json/sportspress/v2/"
  full_url <- paste0(base_url,
                     "tables")

  res <- httr::RETRY("GET", full_url)

  data <- res %>%
    httr::content(as = "text", encoding="utf-8") %>%
    jsonlite::fromJSON()

  # nms <- names(data[[1]]$data)
  standings <- do.call(rbind.data.frame, data$data) %>%
    tibble::rownames_to_column(var = "team_id") %>%
    dplyr::filter(team_id > 0) %>%
    dplyr::mutate(strk = stringr::str_remove(stringr::str_remove(strk, '<span style="color:#888888">'), "</span>"),
                  name = ifelse(stringr::str_detect(name, "Harvey"), "Harvey's", name),
                  team_id = as.numeric(team_id),
                  gp = as.numeric(gp),
                  w = as.numeric(w),
                  l = as.numeric(l),
                  otw = as.numeric(otw),
                  ot = as.numeric(ot),
                  pts = as.numeric(pts),
                  gf = as.numeric(gf),
                  ga = as.numeric(ga),
                  diff = as.numeric(diff),
                  ppg = as.numeric(ppg),
                  ppo = as.numeric(ppo),
                  pppercent = scales::percent(as.numeric(pppercent), scale = 1, accuracy = 0.1),
                  ppga = as.numeric(ppga),
                  ppoa = as.numeric(ppoa),
                  pkpercent = scales::percent(as.numeric(pkpercent), scale = 1, accuracy = 0.1)) %>%
    dplyr::select(name, team_id, pos,
                  gp, w, l, otw, ot,
                  pts, gf, ga, diff,
                  ppg, ppo, pppercent,
                  ppga, ppoa, pkpercent,
                  lfive, strk)

  colnames(standings) <- c("team", "team_id", "rank", "gp", "w", "l", "otw", "ot", "pts", "gf", "ga",
                           "goal_diff", "power_play_goals", "power_plays", "power_play_pct",
                           "power_play_goals_allowed", "power_plays_against", "penalty_kill_pct",
                           "last_five", "streak")

  team_logos <- data.frame(
    team = c("Harvey's", "adidas", "Sonnet", "Scotiabank"),
    team_id = c(157, 130, 182, 89),
    logo = c("https://stats.pwhpa.com/wp-content/uploads/2022/10/harveys-icon-150x150.png",
             "https://stats.pwhpa.com/wp-content/uploads/2022/10/adidas-icon-150x150.png",
             "https://stats.pwhpa.com/wp-content/uploads/2022/10/sonnet-icon-150x150.png",
             "https://stats.pwhpa.com/wp-content/uploads/2022/10/scotiabank-icon-150x150.png")
  )

  standings <- standings %>%
    dplyr::left_join(team_logos, by = c("team", "team_id")) %>%
    tibble::as_tibble()

  return(standings)

}
