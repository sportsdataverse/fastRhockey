library(tidyverse)

process_member_info <- function(player_info) {

  number <- rvest::html_text(player_info[[1]])
  name <- rvest::html_text(player_info[[2]])
  player_link <- player_info[[2]] %>% rvest::html_node("a") %>% rvest::html_attr("href")

  first_name <- stringr::str_split(name, " ")[[1]][1]
  last_name <- stringr::str_split(name, " ")[[1]][2]

  link <- player_info[[3]] %>% rvest::html_node("a") %>% rvest::html_attr("href")
  team_id <- stringr::str_remove(stringr::str_remove(link, "https://stats.pwhpa.com/team/"), "/")

  position <- rvest::html_text(player_info[[4]])
  dob <- rvest::html_text(player_info[[5]])
  age <- rvest::html_text(player_info[[6]])
  home_town <- rvest::html_text(player_info[[7]])
  college <- rvest::html_text(player_info[[8]])

  # player_headshot <- paste0("https://stats.pwhpa.com/wp-content/uploads/2022/10/",
  #                           last_name,
  #                           "-300x300.jpg")

  player_df <- data.frame(
    player_name = c(name),
    first_name = c(first_name),
    last_name = c(last_name),
    team_id = c(team_id),
    position = c(position),
    number = c(number),
    date_of_birth = c(dob),
    age = c(age),
    home_town = c(home_town),
    college = c(college),
    player_page = c(player_link)
  )

  return(player_df)

}

pwhpa_roster <- function(team) {

  full_url <- "https://stats.pwhpa.com/members/"

  res <- httr::RETRY("GET", full_url)

  img <- res %>%
    httr::content(as = "text", encoding="utf-8") %>%
    # purrr::pluck("content") %>%
    rvest::read_html() %>%
    rvest::html_elements("section")

  body <- res %>%
    httr::content(as = "text", encoding="utf-8") %>%
    # purrr::pluck("content") %>%
    rvest::read_html() %>%
    rvest::html_elements("tbody")

  body <- body[[1]] %>% rvest::html_nodes("tr")

  mem <- list()

  for (id in 1:length(body)) {
    player_info <- body[[id]] %>%
      rvest::html_nodes("td")

    player <- process_member_info(player_info = player_info)

    mem[[id]] <- player
  }

  player_data <- dplyr::bind_rows(mem)

  # img[[3]] %>% html_nodes("*") %>% html_attr("class") %>% unique()
  # gallery-icon portrait

  headshot <- img[[3]] %>%
    rvest::html_nodes("img") %>%
    rvest::html_attr("src") %>%
    data.frame() %>%
    dplyr::rename("player_headshot" = ".")

  player_pages <- img[[3]] %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    data.frame() %>%
    dplyr::rename("player_link" = ".") %>%
    distinct() %>%
    dplyr::mutate(index = row_number())

  players <- headshot %>%
    dplyr::slice_head(n = nrow(player_pages)) %>%
    dplyr::mutate(index = row_number()) %>%
    dplyr::left_join(player_pages, by = c("index")) %>%
    dplyr::select(-c(index))

  team_roster <- player_data %>%
    dplyr::left_join(players, by = c("player_page" = "player_link"))

  if (! team %in% c("All", "PWHPA", "all", "pwhpa")) {
    team_roster <- team_roster %>%
      dplyr::filter(team_id == tolower(team))
  }

  team_roster <- team_roster %>%
    tibble::as_tibble()

  return(team_roster)

}
