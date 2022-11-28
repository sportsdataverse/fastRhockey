
library(tidyverse)

process_stats <- function(pos, player_info) {

  if (pos == "skaters") {

    number <- rvest::html_text(player_info[[1]])
    name <- rvest::html_text(player_info[[2]])
    player_link <- player_info[[2]] %>% rvest::html_node("a") %>% rvest::html_attr("href")

    first_name <- stringr::str_split(name, " ")[[1]][1]
    last_name <- stringr::str_split(name, " ")[[1]][2]

    link <- player_info[[3]] %>% rvest::html_node("a") %>% rvest::html_attr("href")
    team_id <- stringr::str_remove(stringr::str_remove(link, "https://stats.pwhpa.com/team/"), "/")

    position <- rvest::html_text(player_info[[4]])
    gp <- rvest::html_text(player_info[[5]])
    g <- rvest::html_text(player_info[[6]])
    a <- rvest::html_text(player_info[[7]])
    pts <- rvest::html_text(player_info[[8]])
    pim <- rvest::html_text(player_info[[9]])

    player_df <- data.frame(
      player_name = c(name),
      first_name = c(first_name),
      last_name = c(last_name),
      team = c(team_id),
      position = c(position),
      number = c(number),
      gp = c(gp),
      goals = c(g),
      assists = c(a),
      points = c(pts),
      penalty_minutes = c(pim)
    )

  } else if (pos == "goalies") {

    number <- rvest::html_text(player_info[[1]])
    name <- rvest::html_text(player_info[[2]])
    player_link <- player_info[[2]] %>% rvest::html_node("a") %>% rvest::html_attr("href")

    first_name <- stringr::str_split(name, " ")[[1]][1]
    last_name <- stringr::str_split(name, " ")[[1]][2]

    link <- player_info[[3]] %>% rvest::html_node("a") %>% rvest::html_attr("href")
    team_id <- stringr::str_remove(stringr::str_remove(link, "https://stats.pwhpa.com/team/"), "/")

    gp <- rvest::html_text(player_info[[4]])
    w <- rvest::html_text(player_info[[5]])
    ga <- rvest::html_text(player_info[[6]])
    gaa <- rvest::html_text(player_info[[7]])
    sa <- rvest::html_text(player_info[[8]])
    svs <- rvest::html_text(player_info[[9]])
    sv_pct <- rvest::html_text(player_info[[10]])
    so <- rvest::html_text(player_info[[11]])
    toi <- rvest::html_text(player_info[[12]])

    player_df <- data.frame(
      player_name = c(name),
      first_name = c(first_name),
      last_name = c(last_name),
      team = c(team_id),
      position = c("G"),
      games = c(gp),
      wins = c(w),
      goals_against = c(ga),
      gaa = c(gaa),
      shots_against = c(sa),
      saves = c(svs),
      save_pct = c(sv_pct),
      shutouts = c(so),
      time_on_ice = c(toi)
    )

  }

  return(player_df)

}

pwhpa_stats <- function(position = "skaters") {

  # https://stats.pwhpa.com/list/scoring-leaders/
  # https://stats.pwhpa.com/list/goaltending/

  base_url <- "https://stats.pwhpa.com/list/"

  if (position == "skaters") {

    full_url <- "https://stats.pwhpa.com/list/scoring-leaders/"

  } else if (position == "goalies") {

    full_url <- "https://stats.pwhpa.com/list/goaltending/"

  }

  res <- httr::RETRY("GET", full_url)

  lst1 <- res %>%
    httr::content(as = "text", encoding="utf-8") %>%
    # purrr::pluck("content") %>%
    rvest::read_html() %>%
    rvest::html_elements("tbody")

  rows <- lst1[[1]] %>% rvest::html_nodes("tr")

  stats <- list()

  for (id in 1:length(rows)) {
    player_info <- rows[[id]] %>%
      rvest::html_nodes("td")

    player <- process_stats(pos = position, player_info = player_info)

    stats[[id]] <- player
  }

  stats <- dplyr::bind_rows(stats) %>%
    tibble::as_tibble()

  return(stats)

}
