library(tidyverse)
library(ggimage)
# library(rvest)
# library(httr)
# library(jsonlite)
# library(rjson)
# library(ggimage)
library(ggtext)

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

  asp_ratio <- 1.618


    standings <-
    standings %>%
      dplyr::mutate(
        gfp = gf / gp,
        gap = ga / gp
      )

  gplt <-
    standings %>%
    ggplot() +
    geom_abline(slope = -1.2,
                intercept = seq(-2, 3),
                color = "grey") +
    geom_vline(xintercept = mean(standings$gf / standings$gp, na.rm = TRUE)) +
    geom_hline(yintercept = mean(standings$ga / standings$gp, na.rm = TRUE)) +
    geom_image(aes(x = gfp, y = gap,
                   image = logo),
               # Set size, and aspect ratio
               size = 0.1, by = "width", asp = asp_ratio) +
    labs(x = "Goals Scored Per Game",
         y = "Goals Allowed Per Game (Reversed)",
         title = "PWHPA Secret Dream Gap Tour\nScoring By Game",
         subtitle = "Montréal, Truro, and Pittsburgh Showcases",
         caption = "Data: fastRhockey · Graphic: Ben Howell (@benhowell71)") +
    geom_richtext(aes(x = max(standings$gfp, na.rm = TRUE) + 0.1, y = min(standings$gap, na.rm = TRUE) - 0.2, label = "GOOD :)"),
                  fill = NA, label.color = NA, size = 6) +
    geom_richtext(aes(x = max(standings$gfp, na.rm = TRUE) + 0.1, y = max(standings$gap, na.rm = TRUE) + 0.2, label = "SCORING!!!"),
                  fill = NA, label.color = NA, size = 6) +
    geom_richtext(aes(x = min(standings$gfp, na.rm = TRUE) - 0.1, y = min(standings$gap, na.rm = TRUE) - 0.2, label = "Boring :/"),
                  fill = NA, label.color = NA, size = 6) +
    geom_richtext(aes(x = min(standings$gfp, na.rm = TRUE) - 0.1, y = max(standings$gap, na.rm = TRUE) + 0.2, label = "BAD :("),
                  fill = NA, label.color = NA, size = 6) +
    scale_x_continuous(limits = c(min(standings$gfp, na.rm = TRUE) - 0.25,
                                  max(standings$gfp, na.rm = TRUE) + 0.25),
                       breaks = c(seq(0,
                                      10,
                                      by = 0.25))) +
    # # scale_y_continuous(limits = c(4, 12),
    # #                    breaks = c(seq(4, 12))) +
    scale_y_reverse(limits = c(max(standings$gap, na.rm = TRUE) + 0.25,
                               min(standings$gap, na.rm = TRUE) - 0.25),
                    breaks = c(seq(10, 0, -0.5))) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      # axis.line = element_line(size = 1),
      panel.border = element_rect(fill = NA),
      axis.text = element_text(face = "bold", size = 14),
      axis.ticks = element_line(size = 0.75),
      axis.ticks.length = unit(0.15, "cm"),
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      axis.title = element_text(size = 12, face = "italic"),
      plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 14),
      plot.caption = element_text(hjust = 0.5, face = "italic", size = 10)
    )

  ggsave(gplt, filename = "pwhpa_goals_plot.png",
         bg = "white",
         height = 8,
         width = 10)

  return(standings)

}

pwhpa_standings()
