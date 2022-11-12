
process_scores <- function(score_info) {

  team <- rvest::html_text(score_info[[1]])
  first <- rvest::html_text(score_info[[2]])
  second <- rvest::html_text(score_info[[3]])
  third <- rvest::html_text(score_info[[4]])
  final <- rvest::html_text(score_info[[5]])
  ppo <- rvest::html_text(score_info[[6]])
  ppg <- rvest::html_text(score_info[[7]])
  sog <- rvest::html_text(score_info[[8]])
  res <- rvest::html_text(score_info[[9]])

  score <- data.frame(
    team = c(team),
    first = c(first),
    second = c(second),
    third = c(third),
    final = c(final),
    power_play_opps = c(ppo),
    power_play_goals = c(ppg),
    shots_on_goal = c(sog),
    result = c(res)
  )

  return(score)

}

pwhpa_boxscore <- function(game_id) {
  # https://stats.pwhpa.com/event/scotiabank-vs-sonnet/

  base_url <- "https://stats.pwhpa.com/event/"
  full_url <- paste0(base_url,
                     game_id,
                     "/")

  # the link for the game + authorization for accessing the API
  res <- httr::RETRY("GET", full_url)

  game <- res %>%
    httr::content(as = "text", encoding="utf-8") %>%
    # purrr::pluck("content") %>%
    rvest::read_html() %>%
    rvest::html_elements("tbody")

  score_g <- list()
  score <- game[[2]] %>% rvest::html_nodes("tr")

  for (id in 1:length(score)) {

    score_info <- score[[id]] %>%
      rvest::html_nodes("td")

    ls <- process_scores(score_info = score_info)

    score_g[[id]] <- ls

  }

  score_line <- dplyr::bind_rows(score_g)

  # off 1
  game[[3]] %>% rvest::html_nodes("tr")
  # def 1
  game[[4]] %>% rvest::html_nodes("tr")

  # off 2
  game[[5]] %>% rvest::html_nodes("tr")
  # def 2
  game[[6]] %>% rvest::html_nodes("tr")

}
