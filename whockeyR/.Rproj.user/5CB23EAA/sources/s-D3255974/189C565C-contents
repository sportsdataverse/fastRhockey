# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

require(tidyverse)
require(rvest)
require(jsonlite)
require(janitor)
require(httr)
require(purrr)
require(stringr)
require(tokenizers)
require(strex)

#### updated tags for what gets removed from the text parsing
away <- "[:digit:] GvA|[:digit:] TkA|[:digit:] Blk"
fill <- "from|by|against|to|and|giveaway|Game"
goalie <- "Starting goalie|Pulled goalie|Returned goalie"
fo <- "faceoff won"
ice <- "Even Strength|Empty Net|Power Play"
shots <- "Snap shot|Wrist shot"
res <- "blocked|saved"
pen <- "Holding|Tripping|Roughing|Hooking|Interference|Slashing|Checking|Ejection"
type <- "Minor|Major"
score_string <- "[:digit:] - [:digit:] [A-Z]+|[:digit:] - [:digit:]"
shoot <- "missed attempt against|scores against|Shootout"

#' @name load_raw_data
#' @description load_raw_data: pull in the raw data for a game_id from the PHF/NWHL API
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @import tidyverse
#' @import rvest
#' @import jsonlite
#' @import janitor
#' @import httr
#' @import purrr
#' @import stringr
#' @import tokenizers
#' @import strex
#' @export
#' @example \dontrun{ df <- load_raw_data(game_id = 268078) }
#### load_raw_data to pull in data from NWHL/PHF API
## game_id = id of specific game that one wants data for
load_raw_data <- function(game_id = 268078) {

  link <- paste0("https://web.api.digitalshift.ca/partials/stats/game/play-by-play?game_id=", game_id)
  # the link for the game + authorization for accessing the API
  data <- httr::GET(link,
                    httr::add_headers(
                      `Authorization`= 'ticket="4dM1QOOKk-PQTSZxW_zfXnOgbh80dOGK6eUb_MaSl7nUN0_k4LxLMvZyeaYGXQuLyWBOQhY8Q65k6_uwMu6oojuO"'
                    )) %>%
    httr::content(as = "text") %>%
    jsonlite::parse_json() %>%
    purrr::pluck("content") %>%
    rvest::read_html() %>%
    rvest::html_table()

}

#' @name process_period
#' @description process_period: processes the raw data of a single period from a PHF game
#'
#' @param data the dataframe of the period that you want parsed into a workable format of pbp data
#' @param period which period of play is this data for? Defaults to 1
#' @import tidyverse
#' @import rvest
#' @import jsonlite
#' @import janitor
#' @import httr
#' @import purrr
#' @import stringr
#' @import tokenizers
#' @import strex
#' @export
#' @example \dontrun{ first_period <- process_period(data = df[[1]], period = 1) }
#### processes the raw data from a period into a workable format
## data = the data frame of raw whockey data
## period = which period you are looking at
process_period <- function(data, period = 1) {

  # the raw data comes in a very very weird format where the only thing we want
  # is every other row so these two lines get us that
  odd <- seq_len(nrow(data)) %% 2
  data <- data[odd == 1, ]

  data <- data %>%
    # make sure that the names of columns are consistent and clean
    janitor::clean_names() %>%
    # create the id for what period it is and what overall event it is
    mutate(event_no = dplyr::row_number(),
           period_id = period) %>%
    rename("event" = "x",
           "description" = "play") %>%
    # only taking events that match to actual on ice stuff
    filter(str_detect(event, 'On Ice') == FALSE) %>%
    # filter(event != "Timeout") %>%
    # dplyr::filter(! grepl("On"))
    janitor::remove_empty(which = c("cols"),
                          quiet = TRUE) %>%
    # since the NWHL/PHF website has an 'expansion' tab for goals
    # that gets put into the description column weirdly
    # so essentially filtering that out
    mutate(description = gsub("{{.*", "", description, perl = TRUE))

  return(data)

}

#' @name process_shootout
#' @description process_shootout: processes the raw data of a shootout from a PHF game
#'
#' @param data the dataframe of the shootout that you want parsed into a workable format of pbp data
#' @import tidyverse
#' @import rvest
#' @import jsonlite
#' @import janitor
#' @import httr
#' @import purrr
#' @import stringr
#' @import tokenizers
#' @import strex
#' @export
#' @example \dontrun{ shootout <- process_shootout(data = game_so) }
process_shootout <- function(data) {

  data <- data %>%
    clean_names() %>%
    # creating variables, cleaning stuff for shootouts specifically
    # since there's a lot less variation in what can happen
    # it's easier to do the cleaning so it's in its own function
    mutate(event = "Shootout",
           on_ice_situation = "shootout",
           shot_type = "shootout",
           shot_result = tolower(x),
           period_id = 5,
           event_no = row_number(),
           description = play,
           desc = str_replace_all(play, "#", ""),
           first_number = str_nth_number(desc, 1),
           second_number = str_nth_number(desc, 2),
           desc = str_replace_all(desc, shoot, ""),
           score = str_extract(desc, score_string),
           desc = str_replace_all(desc, score_string, ""),
           desc = str_replace_all(str_trim(desc, side = "both"),"#", ""),
           first_player = str_nth_non_numeric(desc, n = 1),
           second_player = str_nth_non_numeric(desc, n = 2),
           leader = str_extract(score, "[A-Z]+"),
           scr = str_replace_all(score, "[A-Z]+", "")) %>%
    dplyr::select(-play) %>%
    separate(scr, into = c("away_goals", "home_goals"),
             sep = " - ", remove = FALSE) %>%
    dplyr::select(-c(scr, x)) %>%
    mutate(leader = ifelse(is.na(leader), 'T', leader),
           away_goals = ifelse(is.na(away_goals), 0, away_goals),
           home_goals = ifelse(is.na(home_goals), 0, home_goals),
           score = ifelse(is.na(score), '0 - 0 T', score))

  return(data)

}

#' @name pbp_data
#' @description pbp_data: returns all of the play-by-play data for a game into on big data frame using the process_period/shootout functions. Contains functionality to account for regulation games, overtime games, and shootouts
#'
#' @param data the raw list data that is generated from the load_raw_data function
#' @import tidyverse
#' @import rvest
#' @import jsonlite
#' @import janitor
#' @import httr
#' @import purrr
#' @import stringr
#' @import tokenizers
#' @import strex
#' @export
#' @example \dontrun{ pbp_df <- pbp_data(data = df) }
#### function returning all the pbp data for a game into one big data frame for the game
## data takes the raw list of data from the load_raw_data function
pbp_data <- function(data) {

  # l <- length(data) - 2
  # c <- data[[l]]
  # c <- c %>% clean_names()
  # y <- sum(c$x1st) + 1
  # z <- y + sum(c$x2nd)

  lst <- list()
  # creating an empty list

  # so, since there's not a consistent format of which table in the list the period data is in
  # I have to have it loop through the number of rows in each of those tables
  # then we take each one that has at least 6 observations
  for (y in 1:length(data)) {

    z <- nrow(data[[y]])
    tb <- data.frame(y, z)

    lst[[y]] <- tb

  }
  # 6 observations bc the shootout format is 3 shots per team at minimum
  # since there are 7 lines in one of the boxscore tabs, we have to be careful
  # however, that is always one of the last tables so we can just take the first five
  tb <- dplyr::bind_rows(lst) %>%
    # LOOK AT
    # z > 7 runs easy, but it's possible for a shootout to be only 6 or 7 events
    # and since there's another table that is 7 (boxscore data) it causes errors when I left it
    # as >= 6 so I changed it for now to work
    dplyr::filter(z > 7) %>%
    mutate(order = row_number()) %>%
    filter(order > 0 & order < 6)

  tm <- data[[max(length(data)) - 1]] %>%
    # taking the second to last table bc that is always shots (or goals? now I don't remember)
    # either way, the away team is always on top so we can extract home/away from this
    clean_names() %>%
    mutate(
      order = row_number(),
      meta = ifelse(
        order == 1, "away_team", ifelse(
          order == 2, "home_team", NA
        )
      )
    ) %>%
    dplyr::select(shots, meta) %>%
    pivot_wider(values_from = shots,
                names_from = meta)

  # creating the pbp dataframes for regulation, OT, or shootout games
  if (nrow(tb) == 3) {

    e <- tb %>% filter(order == 1) %>% pull(y)
    f <- tb %>% filter(order == 2) %>% pull(y)
    g <- tb %>% filter(order == 3) %>% pull(y)

    first_period <- data[[e]]
    second_period <- data[[f]]
    third_period <- data[[g]]

    # second_period <- data[[2]]
    # third_period <- data[[4]]

    first_period <- process_period(data = first_period, period = 1)

    second_period <- process_period(data = second_period, period = 2)

    third_period <- process_period(data = third_period, period = 3)

    pbp <- dplyr::bind_rows(first_period,
                            second_period,
                            third_period)

  } else if (nrow(tb) == 4) {

    e <- tb %>% filter(order == 1) %>% pull(y)
    f <- tb %>% filter(order == 2) %>% pull(y)
    g <- tb %>% filter(order == 3) %>% pull(y)
    h <- tb %>% filter(order == 4) %>% pull(y)

    first_period <- data[[e]]
    second_period <- data[[f]]
    third_period <- data[[g]]
    fourth_period <- data[[h]]

    # second_period <- data[[2]]
    # third_period <- data[[4]]

    first_period <- process_period(data = first_period, period = 1)

    second_period <- process_period(data = second_period, period = 2)

    third_period <- process_period(data = third_period, period = 3)

    fourth_period <- process_period(data = fourth_period, period = 4)

    pbp <- dplyr::bind_rows(first_period,
                            second_period,
                            third_period,
                            fourth_period)

  } else if (nrow(tb) >= 5) {

    e <- tb %>% filter(order == 1) %>% pull(y)
    f <- tb %>% filter(order == 2) %>% pull(y)
    g <- tb %>% filter(order == 3) %>% pull(y)
    h <- tb %>% filter(order == 4) %>% pull(y)
    i <- tb %>% filter(order == 5) %>% pull(y)

    first_period <- data[[e]]
    second_period <- data[[f]]
    third_period <- data[[g]]
    fourth_period <- data[[h]]
    shootout <- data[[i]]

    # second_period <- data[[2]]
    # third_period <- data[[4]]

    first_period <- process_period(data = first_period, period = 1)

    second_period <- process_period(data = second_period, period = 2)

    third_period <- process_period(data = third_period, period = 3)

    fourth_period <- process_period(data = fourth_period, period = 4)

    pbp <- dplyr::bind_rows(first_period,
                            second_period,
                            third_period,
                            fourth_period)

  }

  pbp <- pbp %>%
    # replacing extraneous words to parse out player names
    mutate(desc = description,
           desc = str_replace_all(desc, fill, ""),
           desc = str_replace_all(desc, away, ""),
           desc = str_replace_all(desc, goalie, ""),
           desc = str_replace_all(desc, fo, ""),
           # replacing some basic stuff
           on_ice_situation = str_extract(desc, ice),
           desc = str_replace_all(desc, ice, ""),
           # cleaning the on-ice situation
           shot_type = str_extract(desc, shots),
           desc = str_replace_all(desc, shots, ""),
           shot_result = ifelse(str_detect(event, "Goal") & event != "Goalie", "made",
                                str_extract(desc, res)),
           desc = str_replace_all(desc, res, ""),
           # cleaning up shot data to get shot type + the result of the shot
           penalty_type = str_extract(desc, type),
           desc = str_replace_all(desc, type, ""),
           penalty_called = str_extract(desc, pen),
           desc = str_replace_all(desc, pen, ""),
           penalty_length = str_extract(desc,
                                        "[:digit:] mins"),
           desc = str_replace_all(desc,
                                  "[:digit:] mins", ""),
           penalty = ifelse(! is.na(penalty_type), 1, 0),
           # cleaning up penalty data
           score = str_extract(desc, score_string),
           # score = ifelse(is.na(score), '0 - 0 T', score),
           # leader = str_extract(score, "[A-Z]+"),
           desc = str_replace_all(desc, score_string, ""),
           desc = str_replace_all(str_trim(desc, side = "both"),"#", ""),
           # cleaning up score data
           first_player = str_trim(str_nth_non_numeric(desc, n = 1)),
           first_number = str_nth_number(desc, n = 1),
           second_player = str_trim(str_nth_non_numeric(desc, n = 2)),
           second_number = str_nth_number(desc, n = 2),
           third_player = str_trim(str_nth_non_numeric(desc, n = 3)),
           third_number = str_nth_number(desc, n = 3)) %>%
    # dplyr::filter(! is.na(time)) %>%
    separate(time, into = c("minute", "second"),
             sep = ":", remove = FALSE) %>%
    mutate(minute_start = as.numeric(minute),
           second_start = as.numeric(second),
           minute = ifelse(19 - minute_start == 19 &
                             60 - second_start == 60, 20,
                           19 - minute_start),
           second = ifelse(60 - second_start == 60, 0,
                           60 - second_start),
           second = ifelse(second < 10, paste0("0", second),
                           paste0(second)),
           clock = paste0(minute, ":", second)) %>%
    dplyr::select(-c(minute, second)) %>%
    mutate(event_no = row_number())

  on_ice <- pbp %>%
    filter(is.na(time)) %>%
    mutate(event_no = event_no - 1) %>%
    dplyr::select(event, team, event_no, period_id) %>%
    mutate(
      team = str_replace_all(team, "#", ""),
      offensive_player_one = str_trim(side = c("both"), str_nth_non_numeric(team, n = 1)),
      offensive_number_one = str_trim(side = c("both"), str_nth_number(team, n = 1)),
      offensive_player_two = str_trim(side = c("both"), str_nth_non_numeric(team, n = 2)),
      offensive_number_two = str_trim(side = c("both"), str_nth_number(team, n = 2)),
      offensive_player_three = str_trim(side = c("both"), str_nth_non_numeric(team, n = 3)),
      offensive_number_three = str_trim(side = c("both"), str_nth_number(team, n = 3)),
      offensive_player_four = str_trim(side = c("both"), str_nth_non_numeric(team, n = 4)),
      offensive_number_four = str_trim(side = c("both"), str_nth_number(team, n = 4)),
      offensive_player_five = str_trim(side = c("both"), str_nth_non_numeric(team, n = 5)),
      offensive_number_five = str_trim(side = c("both"), str_nth_number(team, n = 5))
    ) %>%
    dplyr::select(-c(event, team))

  pbp <- pbp %>%
    left_join(on_ice, by = c("period_id", "event_no")) %>%
    mutate(leader = str_extract(score, "[A-Z]+"),
           scr = str_replace_all(score, "[A-Z]+", "")) %>%
    separate(scr, into = c("away_goals", "home_goals"),
             sep = " - ", remove = FALSE) %>%
    dplyr::select(-c(scr)) %>%
    fill(score) %>%
    fill(leader) %>%
    fill(away_goals) %>%
    fill(home_goals) %>%
    mutate(leader = ifelse(is.na(leader), 'T', leader),
           away_goals = ifelse(is.na(away_goals), 0, away_goals),
           home_goals = ifelse(is.na(home_goals), 0, home_goals),
           score = ifelse(is.na(score), '0 - 0 T', score)) %>%
    mutate(sec_from_start = (60 * minute_start) + second_start,
           sec_from_start = ifelse(period_id == 2, sec_from_start + 1200,
                                   ifelse(period_id == 3, sec_from_start + 2400,
                                          ifelse(period_id == 4, sec_from_start + 3600,
                                                 ifelse(period_id == 5, sec_from_start + 4800,
                                                        sec_from_start)))),
           power_play_seconds = ifelse(! is.na(penalty_length),
                                       as.numeric(str_extract(penalty_length, '[0-9]')) * 60,
                                       NA),
           start_power_play = ifelse(penalty == 1, sec_from_start, NA),
           end_power_play = ifelse(penalty == 1, start_power_play + power_play_seconds, NA)) %>%
    fill(start_power_play) %>%
    fill(end_power_play) %>%
    # ID'ing PP situations by whether the timestamp is within the time passed from when the penalty was given
    # any situation that isn't special, i.e. as a PP or Empty Net get replaced by Even Strenght
    mutate(on_ice_situation = ifelse((sec_from_start >= start_power_play &
                                        sec_from_start <= end_power_play) |
                                       (on_ice_situation == "Power Play"), "Power Play",
                                     on_ice_situation),
           on_ice_situation = replace_na(on_ice_situation, "Even Strength"))

  if (nrow(tb) >= 5) {

    shootout <- process_shootout(data = shootout)

    pbp <- bind_rows(pbp, shootout)

  }

  pbp <- pbp %>%
    left_join(tm, by = character())

  gl <- pbp %>%
    filter(event == "Goalie") %>%
    # filter(str_detect(description, "Starting|Returned"))
    dplyr::select(home_team, away_team, team, description,
                  first_player, event, sec_from_start) %>%
    mutate(
      goalie_change = str_extract(description, "Starting|Returned|Pulled"),
      goalie = ifelse(
        str_detect(team, away_team), "away_goalie",
        ifelse(
          str_detect(team, home_team), "home_goalie", NA
        )
      ),
      first_player = ifelse(goalie_change == "Pulled", "None", first_player)
    ) %>%
    dplyr::select(first_player, sec_from_start, goalie_change, goalie) %>%
    pivot_wider(names_from = goalie,
                values_from = first_player)

  pbp <- pbp %>%
    left_join(gl, by = c("sec_from_start")) %>%
    fill(home_goalie) %>%
    fill(away_goalie) %>%
    dplyr::filter(event != "Goalie") %>%
    mutate(
      home_goalie = ifelse(home_goalie == "None", NA, home_goalie),
      away_goalie = ifelse(away_goalie == "None", NA, away_goalie),
      goalie_involved = ifelse(event %in% c("Goal", "PP Goal", "Shot", "Shot BLK") &
                                 str_detect(team, home_team), away_goalie,
                               ifelse(event %in% c("Goal", "PP Goal", "Shot", "Shot BLK") &
                                        str_detect(team, away_team), home_goalie, NA
                               )
      ),
      time_elapsed = time,
      time_remaining = clock
    )

  return(pbp)

}

#' @name load_pbp
#' @description load_pbp: loads all the play-by-play data for a game into one data frame through just one function
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @import tidyverse
#' @import rvest
#' @import jsonlite
#' @import janitor
#' @import httr
#' @import purrr
#' @import stringr
#' @import tokenizers
#' @import strex
#' @export
#' @example \dontrun{ first_period <- process_period(data = df[[1]], period = 1) }
#### loading all the play-by-play data into one data frame through just one function
## game_id = which game one wants the pbp data for
load_pbp <- function(game_id = 268078, format = "clean") {

  df <- load_raw_data(game_id = game_id)

  pbp <- pbp_data(data = df)

  pbp <- pbp %>%
    filter(! is.na(description)) %>%
    mutate(game_id = game_id)

  if (format == "clean") {

    pbp <- pbp %>%
      dplyr::select(game_id, home_team, away_team, period_id, event_no, description, time_remaining, on_ice_situation,
                    home_goals, away_goals, leader, team, event,
                    first_player, first_number, second_player, second_number, third_player, third_number,
                    shot_type, shot_result, goalie_involved,
                    penalty, penalty_length, penalty_type, penalty_called,
                    offensive_player_one, offensive_player_two, offensive_player_three,
                    offensive_player_four, offensive_player_five,
                    home_goalie, away_goalie)

  }

  return(pbp)

}

