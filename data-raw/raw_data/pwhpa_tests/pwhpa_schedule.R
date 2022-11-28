library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(rjson)
library(ggimage)
library(ggtext)

# team_logos <- data.frame(
#   team = c("Harvey's", "adidas", "Sonnet", "Scotiabank"),
#   team_id = c(157, 130, 182, 89),
#   logo = c("https://stats.pwhpa.com/wp-content/uploads/2022/10/harveys-icon-150x150.png",
#            "https://stats.pwhpa.com/wp-content/uploads/2022/10/adidas-icon-150x150.png",
#            "https://stats.pwhpa.com/wp-content/uploads/2022/10/sonnet-icon-150x150.png",
#            "https://stats.pwhpa.com/wp-content/uploads/2022/10/scotiabank-icon-150x150.png")
# )


process_game_info <- function(game_info) {

  date <- substr(html_text(game_info[[1]]), 0, 19)
  date_text <- substr(html_text(game_info[[1]]), 20, nchar(rvest::html_text(game_info[[1]])))

  home_tm <- stringr::str_trim(rvest::html_text(game_info[[2]]))
  away_tm <- stringr::str_trim(rvest::html_text(game_info[[4]]))

  if (grepl("pm|am", rvest::html_text(game_info[[3]]))) {

    home_scr <- NA
    away_scr <- NA

  } else {

    home_scr <- stringr::str_split(rvest::html_text(game_info[[3]]), " - ", n = 2)[[1]][1]
    away_scr <- stringr::str_split(rvest::html_text(game_info[[3]]), " - ", n = 2)[[1]][2]

  }

  location <- rvest::html_text(game_info[[5]])

  game_link <- game_info[[6]] %>% rvest::html_node("a") %>% rvest::html_attr("href")

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

  schedule_df <- dplyr::bind_rows(lst) %>%
    tibble::as_tibble()

  return(schedule_df)

}

# df <- pwhpa_schedule()

# pwhpa_schedule <- function() {
#
#   base_url <- "https://stats.pwhpa.com/wp-json/sportspress/v2/"
#   full_url <- paste0(base_url,
#                      "calendars")
#
#   res <- httr::RETRY("GET", full_url)
#
#   # res <- fromJSON(full_url)
#
#   # res <- GET(full_url)
#
#   data <- res %>%
#     httr::content(as = "text", encoding="utf-8") %>%
#     fromJSON()
#     # purrr::pluck("content") %>%
#     rvest::read_html() %>%
#     rvest::html_elements("tbody")
#
# }

# pwhpa_standings <- function() {
#
#   base_url <- "https://stats.pwhpa.com/wp-json/sportspress/v2/"
#   full_url <- paste0(base_url,
#                      "tables")
#
#   res <- httr::RETRY("GET", full_url)
#
#   data <- res %>%
#     httr::content(as = "text", encoding="utf-8") %>%
#     fromJSON()
#
#   # nms <- names(data[[1]]$data)
#   standings <- do.call(rbind.data.frame, data[[1]]$data) %>%
#     tibble::rownames_to_column(var = "team_id") %>%
#     dplyr::filter(team_id > 0) %>%
#     dplyr::mutate(strk = stringr::str_remove(stringr::str_remove(strk, '<span style="color:#888888">'), "</span>"),
#                   name = ifelse(stringr::str_detect(name, "Harvey"), "Harvey's", name),
#                   team_id = as.numeric(team_id),
#                   gp = as.numeric(gp),
#                   w = as.numeric(w),
#                   l = as.numeric(l),
#                   otw = as.numeric(otw),
#                   ot = as.numeric(ot),
#                   pts = as.numeric(pts),
#                   gf = as.numeric(gf),
#                   ga = as.numeric(ga),
#                   diff = as.numeric(diff),
#                   ppg = as.numeric(ppg),
#                   ppo = as.numeric(ppo),
#                   pppercent = scales::percent(as.numeric(pppercent), scale = 1),
#                   ppga = as.numeric(ppga),
#                   ppoa = as.numeric(ppoa),
#                   pkpercent = scales::percent(as.numeric(pkpercent), scale = 1)) %>%
#     dplyr::select(name, team_id, pos,
#                   gp, w, l, otw, ot,
#                   pts, gf, ga, diff,
#                   ppg, ppo, pppercent,
#                   ppga, ppoa, pkpercent,
#                   lfive, strk)
#
#   colnames(standings) <- c("team", "team_id", "rank", "gp", "w", "l", "otw", "ot", "pts", "gf", "ga",
#                            "goal_diff", "power_play_goals", "power_plays", "power_play_pct",
#                            "power_play_goals_allowed", "power_plays_against", "penalty_kill_pct",
#                            "last_five", "streak")
#
#   standings <- standings %>%
#     dplyr::left_join(team_logos, by = c("team", "team_id")) %>%
#     tibble()
#
#   return(standings)
#
# }


# asp_ratio <- 1.618
#
# gplt <- standings %>%
#   ggplot() +
#   geom_abline(slope = -1.2,
#               intercept = seq(-2, 3),
#               color = "grey") +
#   geom_vline(xintercept = mean(standings$gf / standings$gp, na.rm = TRUE)) +
#   geom_hline(yintercept = mean(standings$ga / standings$gp, na.rm = TRUE)) +
#   geom_image(aes(x = gf / gp, y = ga / gp,
#                  image = logo),
#              # Set size, and aspect ratio
#              size = 0.1, by = "width", asp = asp_ratio) +
#   labs(x = "Goals Scored Per Game",
#        y = "Goals Allowed Per Game (Reversed)",
#        title = "PWHPA Secret Dream Gap Tour\nScoring By Game",
#        subtitle = "Montréal & Truro Showcases",
#        caption = "Data: fastRhockey · Graphic: Ben Howell (@benhowell71)") +
#   geom_richtext(aes(x = 3.4, y = 1.15, label = "GOOD :)"),
#                 fill = NA, label.color = NA, size = 6) +
#   geom_richtext(aes(x = 3.4, y = 3.85, label = "SCORING!!!"),
#                 fill = NA, label.color = NA, size = 6) +
#   geom_richtext(aes(x = 2.05, y = 1.15, label = "Boring :/"),
#                 fill = NA, label.color = NA, size = 6) +
#   geom_richtext(aes(x = 2.05, y = 3.85, label = "BAD :("),
#                 fill = NA, label.color = NA, size = 6) +
#   scale_x_continuous(limits = c(2, 3.5),
#                      breaks = c(seq(2, 3.5, by = 0.25))) +
#   # scale_y_continuous(limits = c(4, 12),
#   #                    breaks = c(seq(4, 12))) +
#   scale_y_reverse(limits = c(4, 1),
#                   breaks = c(seq(4, 1, -0.5))) +
#   theme_minimal() +
#   theme(
#     panel.grid = element_blank(),
#     # axis.line = element_line(size = 1),
#     panel.border = element_rect(fill = NA),
#     axis.text = element_text(face = "bold", size = 14),
#     axis.ticks = element_line(size = 0.75),
#     axis.ticks.length = unit(0.15, "cm"),
#     plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
#     axis.title = element_text(size = 12, face = "italic"),
#     plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 14),
#     plot.caption = element_text(hjust = 0.5, face = "italic", size = 10)
#   )
#
# ggsave(gplt, filename = "pwhpa_goals_plot.png",
#        bg = "white",
#        height = 8,
#        width = 10)
