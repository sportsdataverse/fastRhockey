library(tidyverse)
library(rvest)
library(httr)

process_roster <- function(player) {

  number <- html_text(player[[1]])
  name <- html_text(player[[2]])
  first_name <- stringr::str_split(name, " ")[[1]][1]
  last_name <- stringr::str_split(name, " ")[[1]][2]
  position <- html_text(player[[3]])
  hand <- html_text(player[[4]])
  gp <- html_text(player[[5]])
  g <- html_text(player[[6]])
  a <- html_text(player[[7]])
  pts <- html_text(player[[8]])
  pim <- html_text(player[[9]])
  gw <- html_text(player[[10]])
  gaa <- html_text(player[[11]])
  sv_pct <- html_text(player[[12]])
  so <- html_text(player[[13]])
  home_town <- html_text(player[[14]])
  college <- html_text(player[[15]])

  player_df <- data.frame(
    player_name = c(name),
    first_name = c(first_name),
    last_name = c(last_name),
    position = c(position),
    number = c(number),
    hand = c(hand),
    gp = c(gp),
    goals = c(g),
    assists = c(a),
    points = c(pts),
    penalty_minutes = c(pim),
    games_won = c(gw),
    gaa = c(gaa),
    save_pct = c(sv_pct),
    shutouts = c(so),
    home_town = c(home_town),
    college = c(college)
  )

  return(player_df)

}

pwhpa_team_stats <- function(team) {

  base_url <- "https://stats.pwhpa.com/team/"
  full_url <- paste0(base_url,
                     team,
                     "/")

  res <- httr::RETRY("GET", full_url)

  lst1 <- res %>%
    httr::content(as = "text", encoding="utf-8") %>%
    # purrr::pluck("content") %>%
    rvest::read_html() %>%
    rvest::html_elements("tbody")

  tms <- lst1[[2]] %>%
    rvest::html_nodes("tr")

  lst <- list()

  for (id in 1:length(tms)) {

    line <- tms[[id]] %>% html_nodes("td")

    # print(length(line))
    if (length(line) > 0) {
      if (str_detect(paste(line[[1]]), "data-number")) {

        player <- process_roster(player = line) %>%
          dplyr::mutate(team = team,
                        season = 2022)

      } else {
        next
      }
    } else {
      next
    }

    lst[[id]] <- player

  }

  roster <- dplyr::bind_rows(lst) %>%
    tibble()

  return(roster)

}
