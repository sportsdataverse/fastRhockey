library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(rjson)

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

process_offense <- function(player) {

  number <- rvest::html_text(player[[1]])
  name <- rvest::html_text(player[[2]])
  pos <- rvest::html_text(player[[3]])
  goals <- rvest::html_text(player[[4]])
  assists <- rvest::html_text(player[[5]])
  pen_min <- rvest::html_text(player[[6]])

  df <- data.frame(
    player = c(name),
    number = c(number),
    position = c(pos),
    goals = c(goals),
    assists = c(assists),
    penalty_minutes = c(pen_min)
  )

  return(df)

}

process_offense_players <- function(off_players) {

  off <- list()

  for (id in 1:length(off_players)) {

    single_player <- off_players[[id]] %>%
      rvest::html_nodes("td") %>%
      process_offense()

    off[[id]] <- single_player

  }

  offense <- dplyr::bind_rows(off) %>%
    dplyr::mutate(
      star_of_game = stringr::str_extract(player, "[0-9]+"),
      star_of_game = as.numeric(star_of_game),
      player = stringr::str_trim(stringr::str_remove(player, "[0-9]+"))
    )

  return(offense)

}

process_defense <- function(single_defense) {

  number <- rvest::html_text(single_defense[[1]])
  name <- rvest::html_text(single_defense[[2]])
  pos <- rvest::html_text(single_defense[[3]])
  winner <- rvest::html_text(single_defense[[4]])
  ga <- rvest::html_text(single_defense[[5]])
  sa <- rvest::html_text(single_defense[[6]])
  sv <- rvest::html_text(single_defense[[7]])
  so <- rvest::html_text(single_defense[[8]])
  toi <- rvest::html_text(single_defense[[9]])

  goalie <- data.frame(
    player = c(name),
    number = c(number),
    position = c(pos),
    winner = c(winner),
    shutout = c(so),
    goals_allowed = c(ga),
    shots_against = c(sa),
    saves = c(sv),
    time_on_ice = c(toi)
  )

  return(goalie)

}

process_defense_players <- function(def_players) {

  def <- list()

  for (id in 1:length(def_players)) {

    single_def <- def_players[[id]] %>%
      rvest::html_nodes("td") %>%
      process_defense()

    def[[id]] <- single_def

  }

  defense <- dplyr::bind_rows(def) %>%
    dplyr::mutate(
      star_of_game = stringr::str_extract(player, "[0-9]+"),
      star_of_game = as.numeric(star_of_game),
      player = stringr::str_trim(stringr::str_remove(player, "[0-9]+"))
    )

  return(defense)

}

process_scoring_events <- function(score) {

  df <- list()

  event_list <- stringr::str_split(rvest::html_text(score), pattern = "\n")

  for (play in 1:length(event_list[[1]])) {

    if (nchar(event_list[[1]][play]) > 0) {

      # team <- stringr::str_extract(event_list[[1]][play], "Scotiabank|Sonnet|Harvey's|adidas")
      df[[play]] <- data.frame(
        event = c(event_list[[1]][play])
      )

    } else {
      next
    }

  }

  df <- dplyr::bind_rows(df) %>%
    tidyr::separate(event, into = c("team", "period", "time", "players"),
                    remove = FALSE, sep = "-") %>%
    dplyr::mutate(
      period = stringr::str_trim(stringr::str_remove_all(period, "st|nd|rd|th|period")),
      # scorer = stringr::str_extract(players, "^\\()"),
      game_state = stringr::str_extract(players, "EN|PP|ES|SH"),
      players = stringr::str_remove(players, "\\(EN\\)|\\(PP\\)|\\(ES\\)|\\(SH\\)"),
      assist = stringr::str_remove_all(stringr::str_extract(players, "\\(([^()]+)\\)"), "\\(|\\)"),
      players = stringr::str_remove(players, "\\(([^()]+)\\)"),
      assist = ifelse(assist == "Unassisted", NA, assist),
      primary_assist = stringr::str_extract(assist, "[^,]+"),
      secondary_assist = stringr::str_replace(assist, "[^,]+", replacement = ""),
      secondary_assist = stringr::str_replace(secondary_assist, ",", replacement = ""),
      secondary_assist = ifelse(secondary_assist %in% c(""), NA, secondary_assist),
      # assist = stringr::str_extract(players, "\\([A-z]+\\)"),
      across(c("team", "period", "time", "players", "game_state",
               "assist", "primary_assist", "secondary_assist"), stringr::str_trim)) %>%
    dplyr::select(-c(assist, event))
    # dplyr::mutate(
    #   stringr::str_split_fixed(event, "-", n = 4)
    # ) %>%
    # janitor::clean_names()

  # colnames(df) <- c("event", "team", "period", "time", "players")

  return(df)

}

process_penalty_events <- function(penalty) {

  df <- list()

  event_list <- stringr::str_split(rvest::html_text(penalty), pattern = "\n")

  for (play in 1:length(event_list[[1]])) {

    if (nchar(event_list[[1]][play]) > 0) {

      # team <- stringr::str_extract(event_list[[1]][play], "Scotiabank|Sonnet|Harvey's|adidas")
      df[[play]] <- data.frame(
        event = c(event_list[[1]][play])
      )

    } else {
      next
    }

  }

  dplyr::bind_rows(df) %>%
    dplyr::mutate(
      team = stringr::str_extract_all(event, "Scotiabank|Sonnet|Harvey's|adidas"),
      event = stringr::str_remove(event, "Scotiabank|Sonnet|Harvey's|adidas"),
      period = stringr::str_extract(event, "2nd|3rd|1st|")
    )

    tidyr::separate(event, into = c("team", "period", "time", "player", "penalty"),
                    remove = FALSE, sep = "-")

}

pwhpa_boxscore <- function(game_id) {
  # https://stats.pwhpa.com/event/scotiabank-vs-sonnet/

  base_url <- "https://stats.pwhpa.com/event/"
  full_url <- paste0(base_url,
                     game_id,
                     "/")

  # the link for the game + authorization for accessing the API
  res <- httr::RETRY("GET", full_url)

  base <- res %>%
    httr::content(as = "text", encoding="utf-8") %>%
    # purrr::pluck("content") %>%
    rvest::read_html()

  game <- base %>%
    rvest::html_elements("tbody")

  score_g <- list()
  score <- game[[2]] %>% rvest::html_nodes("tr")

  for (id in 1:length(score)) {

    score_info <- score[[id]] %>%
      rvest::html_nodes("td")

    ls <- process_scores(score_info = score_info)

    score_g[[id]] <- ls

  }

  score_line <- dplyr::bind_rows(score_g) %>%
    tibble::as_tibble()

  # off 1
  off1 <- game[[3]] %>% rvest::html_nodes("tr") %>%
    process_offense_players() %>%
    dplyr::mutate(team = score_line$team[[1]])
  # def 1
  def1 <- game[[4]] %>% rvest::html_nodes("tr") %>%
    process_defense_players() %>%
    dplyr::mutate(team = score_line$team[[1]])

  # off 2
  off2 <- game[[5]] %>% rvest::html_nodes("tr") %>%
    process_offense_players() %>%
    dplyr::mutate(team = score_line$team[[2]])
  # def 2
  def2 <- game[[6]] %>% rvest::html_nodes("tr") %>%
    process_defense_players() %>%
    dplyr::mutate(team = score_line$team[[2]])

  offense_stats <- dplyr::bind_rows(off1, off2) %>%
    tibble::as_tibble()
  defense_stats <- dplyr::bind_rows(def1, def2) %>%
    tibble::as_tibble()

  # headers <- base %>%
  #   rvest::html_elements("ul")
  #
  # score <- headers[[5]] %>%
  #   process_scoring_events()
  # penalty <- headers[[6]]

  results <- list(
    "team_scores" = score_line,
    "skaters" = offense_stats,
    "goalies" = defense_stats
  )

  return(results)

}
