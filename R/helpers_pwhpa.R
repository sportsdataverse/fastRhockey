#' @title process_scores to normalize PWHPA scoring data and boxscores
#' @description helper function to fix team points by period data
#' @param score_info is the html_object from the PWHPA website
#' @return Returns a data frame to be used in the rest of the functions
#' @keywords process PWHPA scores
#' @importFrom rvest html_text
#' @noRd
process_scores <- function(score_info) {

  if (length(score_info) == 9) {
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
  } else if (length(score_info) == 3) {
    team <- rvest::html_text(score_info[[1]])
    final <- rvest::html_text(score_info[[2]])
    res <- rvest::html_text(score_info[[3]])

    score <- data.frame(
      team = c(team),
      first = c(NA),
      second = c(NA),
      third = c(NA),
      final = c(final),
      power_play_opps = c(NA),
      power_play_goals = c(NA),
      shots_on_goal = c(NA),
      result = c(res)
    )
  }

  return(score)

}

#' @title process_offense
#' @description helper function to normalize postgame offense stats
#' @param player takes an individual player's line from the parsed HTML as an argument
#' @return Returns a data frame to be used in the rest of the functions
#' @keywords process PWHPA offense boxscore information
#' @importFrom rvest html_text
#' @noRd
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

#' @title process_offense
#' @description helper function to normalize postgame offense stats
#' @param off_players takes all the offensive players information from the parsed HTML
#' @return Returns a data frame to be used in the rest of the functions
#' @keywords process PWHPA offense boxscore information
#' @importFrom rvest html_text html_nodes
#' @importFrom dplyr bind_rows mutate
#' @importFrom stringr str_extract str_trim str_remove
#' @noRd
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

#' @title process_defense
#' @description helper function to normalize postgame goalie stats
#' @param single_defense the node of a single player's defense
#' @return Returns a data frame to be used in the rest of the functions
#' @keywords process PWHPA goalie boxscore information
#' @importFrom rvest html_text
#' @noRd
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

#' @title proces_defense_players
#' @description helper function to normalize postgame goalie stats
#' @return Returns a data frame to be used in the rest of the functions
#' @keywords process PWHPA goalie boxscore information
#' @importFrom rvest html_nodes
#' @importFrom dplyr bind_rows mutate
#' @importFrom stringr str_extract str_trim str_remove
#' @noRd
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

#' @title process_scoring_events
#' @description helper function to normalize scoring and goals scored data
#' @param score takes the scoring HTML content
#' @return Returns a data frame to be used in the rest of the functions
#' @keywords process PWHPA scoring information
#' @importFrom rvest html_text
#' @importFrom stringr str_split str_trim str_remove_all str_extract str_replace
#' @importFrom dplyr bind_rows mutate across select
#' @importFrom tidyr separate
#' @noRd
process_scoring_events <- function(score) {

  df <- list()

  event_list <- stringr::str_split(rvest::html_text(score), pattern = "\n")

  for (play in 1:length(event_list[[1]])) {

    if (nchar(event_list[[1]][play]) > 0) {

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
      game_state = stringr::str_extract(players, "EN|PP|ES|SH"),
      players = stringr::str_remove(players, "\\(EN\\)|\\(PP\\)|\\(ES\\)|\\(SH\\)"),
      assist = stringr::str_remove_all(stringr::str_extract(players, "\\(([^()]+)\\)"), "\\(|\\)"),
      players = stringr::str_remove(players, "\\(([^()]+)\\)"),
      assist = ifelse(assist == "Unassisted", NA, assist),
      primary_assist = stringr::str_extract(assist, "[^,]+"),
      secondary_assist = stringr::str_replace(assist, "[^,]+", replacement = ""),
      secondary_assist = stringr::str_replace(secondary_assist, ",", replacement = ""),
      secondary_assist = ifelse(secondary_assist %in% c(""), NA, secondary_assist),
      dplyr::across(c("team", "period", "time", "players", "game_state",
               "assist", "primary_assist", "secondary_assist"), stringr::str_trim)) %>%
    dplyr::select(-c(assist, event))

  return(df)

}

#' @title process_penalty_events
#' @description helper function to normalize penalty data
#' @param penalty takes the penalty HTML content
#' @return Returns a data frame to be used in the rest of the functions
#' @keywords process PWHPA penalty information
#' @importFrom rvest html_text
#' @importFrom stringr str_split str_trim str_remove_all str_extract str_replace str_extract_all
#' @importFrom dplyr bind_rows mutate across select
#' @importFrom tidyr separate
#' @noRd
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

#' @title process_member_info
#' @description helper function to process player information
#' @param player_info parses a given player's information into a dataframe
#' @return Returns a data frame to be used in the rest of the functions
#' @keywords process PWHPA roster information
#' @importFrom rvest html_text html_node html_attr
#' @importFrom stringr str_split str_trim str_remove_all str_extract str_replace
#' @noRd
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

#' @title process_game_info
#' @description helper function to process game information
#' @param game_info parses a given game's information into a dataframe
#' @return Returns a data frame to be used in the rest of the functions
#' @keywords process PWHPA game information
#' @importFrom rvest html_text html_node html_attr
#' @importFrom stringr str_split str_trim str_remove_all str_extract str_replace
#' @noRd
process_game_info <- function(game_info) {

  date <- substr(rvest::html_text(game_info[[1]]), 0, 19)
  date_text <- substr(rvest::html_text(game_info[[1]]), 20, nchar(rvest::html_text(game_info[[1]])))

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

#' @title process_roster
#' @description helper function to process roster/stat information
#' @param player parses a given player's information into a dataframe
#' @return Returns a data frame to be used in the rest of the functions
#' @keywords process PWHPA player/roster information
#' @importFrom rvest html_text html_node html_attr
#' @importFrom stringr str_split str_trim str_remove_all str_extract str_replace
#' @noRd
process_roster <- function(player) {

  number <- rvest::html_text(player[[1]])
  name <- rvest::html_text(player[[2]])
  first_name <- stringr::str_split(name, " ")[[1]][1]
  last_name <- stringr::str_split(name, " ")[[1]][2]
  position <- rvest::html_text(player[[3]])
  hand <- rvest::html_text(player[[4]])
  gp <- rvest::html_text(player[[5]])
  g <- rvest::html_text(player[[6]])
  a <- rvest::html_text(player[[7]])
  pts <- rvest::html_text(player[[8]])
  pim <- rvest::html_text(player[[9]])
  gw <- rvest::html_text(player[[10]])
  gaa <- rvest::html_text(player[[11]])
  sv_pct <- rvest::html_text(player[[12]])
  so <- rvest::html_text(player[[13]])
  home_town <- rvest::html_text(player[[14]])
  college <- rvest::html_text(player[[15]])

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

#' @title process_roster
#' @description helper function to process roster/stat information
#' @param pos identifies the position of the player
#' @param player_info contains the HTML content for the player
#' @return Returns a data frame to be used in the rest of the functions
#' @keywords process PWHPA player/roster information
#' @importFrom rvest html_text html_node html_attr
#' @importFrom stringr str_split str_trim str_remove_all str_extract str_replace
#' @noRd
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
