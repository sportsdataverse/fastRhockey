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
abbreviations <- c("TOR", "MIN","")
#' @title phf_game_data
#' @description phf_game_data: pull in the raw data for a game_id from the PHF/NWHL API
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @import rvest
#' @importFrom jsonlite parse_json
#' @importFrom purrr pluck
#' @export
#' @examples
#' \dontrun{
#'   phf_game_data(game_id = 368722)
#' }
phf_game_data <- function(game_id = 368719) {
  base_url <- "https://web.api.digitalshift.ca/partials/stats/game/play-by-play?game_id="
  full_url <- paste0(base_url, game_id)
  # the link for the game + authorization for accessing the API
  res <- httr::RETRY(
    "GET", full_url,
    httr::add_headers(
      `Authorization`='ticket="4dM1QOOKk-PQTSZxW_zfXnOgbh80dOGK6eUb_MaSl7nUN0_k4LxLMvZyeaYGXQuLyWBOQhY8Q65k6_uwMu6oojuO"'))
  # Check the result
  check_status(res)
  plays_data <- data.frame()
  tryCatch(
    expr={
      data <- res %>%
        httr::content(as = "text", encoding="utf-8") %>%
        jsonlite::fromJSON() %>%
        purrr::pluck("content") %>%
        rvest::read_html() %>%
        rvest::html_table()

      plays_data <- data[
        !sapply(
          lapply(data, function(x){
            if("Time" %in% colnames(x) && nrow(x)>0){
              return(x)
            }
            if("Play" %in% colnames(x) && nrow(x)>0){
              return(x)
            }
          }),is.null)]
      if(length(plays_data)>5){
        plays_data <- plays_data[1:5]
      }
      plays_df <- purrr::map_dfr(1:length(plays_data), function(x){
        plays_data[[x]] %>%
          normalize_columns() %>%
          dplyr::mutate(period_id = x)
      })
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid game_id or no game data available!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(plays_data)
}

#' @title normalize_columns
#' @description First in processing pipeline to give normalized columns:
#' play_type, team, time, play_description,
#' scoring_team_abbrev, scoring_team_on_ice,
#' defending_team_abbrev, defending_team_on_ice
#'
#' @param df play-by-play data frame
#' @importFrom dplyr mutate mutate_at bind_cols lead filter select
#' @importFrom stringr str_detect
#'
normalize_columns <- function(df){
  if(ncol(df)==3){
    colnames(df) <- c("play_type","team","play_description")
    df$time <- NA_character_
  }
  if(ncol(df)==10){

    colnames(df) <- c("play_type", "team", "time","play_description","drop1","drop2",
                      "scoring_team_abbrev","scoring_team_on_ice","defending_team_abbrev","defending_team_on_ice")
    df2 <- df[,7:10]
    df2 <- df2 %>%
      dplyr::mutate_at(1:4, function(x){dplyr::lead(x,n=2)})
    df <- dplyr::bind_cols(df[,1:4], df2)
    df <- df %>%
      dplyr::filter(!is.na(.data$play_description),!stringr::str_detect(.data$team,"On Ice")) %>%
      dplyr::mutate(play_description = gsub("{{.*", "", .data$play_description, perl = TRUE))
  }else{
    colnames(df) <- c("play_type", "team", "time","play_description")
    df$scoring_team_abbrev <- NA_character_
    df$scoring_team_on_ice <- NA_character_
    df$defending_team_abbrev <- NA_character_
    df$defending_team_on_ice <- NA_character_
    df <- df %>%
      dplyr::filter(!is.na(.data$play_description),!stringr::str_detect(.data$team,"On Ice")) %>%
      dplyr::mutate(play_description = gsub("{{.*", "", .data$play_description, perl = TRUE))
  }
  df <- df %>%
    dplyr::select(.data$play_type, .data$team, .data$time, .data$play_description,
                  .data$scoring_team_abbrev,.data$scoring_team_on_ice,
                  .data$defending_team_abbrev, .data$defending_team_on_ice)
  return(df)
}


#' @title process_period
#' @description process_period: processes the raw data of a single period from a PHF game
#'
#' @param data the dataframe of the period that you want parsed into a workable format of pbp data
#' @param period which period of play is this data for? Defaults to 1
#' @importFrom dplyr mutate row_number rename filter
#' @importFrom janitor clean_names remove_empty
#' @importFrom stringr str_detect
#' @export
#' @examples
#' \donttest{
#'   first_period <- process_period(data = df[[1]], period = 1)
#' }
process_period <- function(data, period = 1) {

  # the raw data comes in a very very weird format where the only thing we want
  # is every other row so these two lines get us that
  odd <- seq_len(nrow(data)) %% 2
  data <- data[odd == 1, ]

  data <- data %>%
    # make sure that the names of columns are consistent and clean
    janitor::clean_names() %>%
    # create the id for what period it is and what overall event it is
    dplyr::mutate(
      event_no = dplyr::row_number(),
      period_id = period) %>%
    dplyr::rename(
      event = .data$x,
      description = .data$play) %>%
    # only taking events that match to actual on ice stuff
    dplyr::filter(!stringr::str_detect(.data$event, 'On Ice')) %>%
    # filter(event != "Timeout") %>%
    # dplyr::filter(! grepl("On"))
    janitor::remove_empty(which = c("cols"), quiet = TRUE) %>%
    # since the NWHL/PHF website has an 'expansion' tab for goals
    # that gets put into the description column weirdly
    # so essentially filtering that out
    dplyr::mutate(
      description = gsub("{{.*", "", .data$description, perl = TRUE))

  return(data)

}


#' @title load_raw_data
#' @description load_raw_data: pull in the raw data for a game_id from the PHF/NWHL API
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @export
#' @examples
#' \dontrun{
#'   df <- load_raw_data(game_id = 268078)
#' }
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
#' @title process_shootout
#' @description process_shootout: processes the raw data of a shootout from a PHF game
#'
#' @param data the dataframe of the shootout that you want parsed into a workable format of pbp data
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate row_number select
#' @importFrom stringr str_extract str_replace_all
#' @importFrom tidyr separate
#' @import tokenizers
#' @import strex
#' @export
#' @examples
#' \dontrun{
#'   shootout <- process_shootout(data = game_so)
#' }
process_shootout <- function(data) {
  score_string <- "[:digit:] - [:digit:] [A-Z]+|[:digit:] - [:digit:]"
  shoot <- "missed attempt against|scores against|Shootout"

  data <- data %>%
    janitor::clean_names() %>%
    # creating variables, cleaning stuff for shootouts specifically
    # since there's a lot less variation in what can happen
    # it's easier to do the cleaning so it's in its own function
    dplyr::mutate(
      event = "Shootout",
      on_ice_situation = "shootout",
      shot_type = "shootout",
      shot_result = tolower(.data$x),
      period_id = 5,
      event_no = dplyr::row_number(),
      description = .data$play,
      desc = stringr::str_replace_all(.data$play, "#", ""),
      first_number = str_nth_number(.data$desc, 1),
      second_number = str_nth_number(.data$desc, 2),
      desc = str_replace_all(.data$desc, shoot, ""),
      score = str_extract(.data$desc, score_string),
      desc = str_replace_all(.data$desc, score_string, ""),
      desc = str_replace_all(str_trim(.data$desc, side = "both"),"#", ""),
      first_player = str_nth_non_numeric(.data$desc, n = 1),
      second_player = str_nth_non_numeric(.data$desc, n = 2),
      leader = str_extract(.data$score, "[A-Z]+"),
      scr = str_replace_all(.data$score, "[A-Z]+", "")) %>%
    dplyr::select(-.data$play) %>%
    tidyr::separate(
      .data$scr,
      into = c("away_goals", "home_goals"),
      sep = " - ", remove = FALSE) %>%
    dplyr::select(-.data$scr, -.data$x) %>%
    dplyr::mutate(
      leader = ifelse(is.na(.data$leader), 'T', .data$leader),
      away_goals = ifelse(is.na(.data$away_goals), 0, .data$away_goals),
      home_goals = ifelse(is.na(.data$home_goals), 0, .data$home_goals),
      score = ifelse(is.na(.data$score), '0 - 0 T', .data$score))

  return(data)

}

#' @title pbp_data
#' @description pbp_data: returns all of the play-by-play data for a game into on big data frame using the process_period/shootout functions. Contains functionality to account for regulation games, overtime games, and shootouts
#'
#' @param data the raw list data that is generated from the load_raw_data function
#' @importFrom dplyr mutate bind_rows filter row_number select case_when pull
#' @importFrom tidyr pivot_wider
#' @import rvest
#' @import jsonlite
#' @import tokenizers
#' @import strex
#' @export
#' @examples \dontrun{
#'   pbp_df <- pbp_data(data = df)
#' }
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
    dplyr::filter(.data$z > 7) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    dplyr::filter(.data$order > 0, .data$order < 6)

  tm <- data[[max(length(data)) - 1]] %>%
    # taking the second to last table bc that is always shots (or goals? now I don't remember)
    # either way, the away team is always on top so we can extract home/away from this
    janitor::clean_names() %>%
    dplyr::mutate(
      order = dplyr::row_number(),
      meta = dplyr::case_when(
        .data$order == 1 ~ "away_team",
        .data$order == 2 ~ "home_team",
        TRUE ~ NA_character_)) %>%
    dplyr::select(.data$shots, .data$meta) %>%
    tidyr::pivot_wider(values_from = .data$shots, names_from = .data$meta)

  # creating the pbp dataframes for regulation, OT, or shootout games
  if (nrow(tb) == 3) {

    e <- tb %>%
      dplyr::filter(.data$order == 1) %>%
      dplyr::pull(.data$y)
    f <- tb %>%
      dplyr::filter(.data$order == 2) %>%
      dplyr::pull(.data$y)
    g <- tb %>%
      dplyr::filter(.data$order == 3) %>%
      dplyr::pull(.data$y)

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

    e <- tb %>%
      dplyr::filter(.data$order == 1) %>%
      dplyr::pull(.data$y)
    f <- tb %>%
      dplyr::filter(.data$order == 2) %>%
      dplyr::pull(.data$y)
    g <- tb %>%
      dplyr::filter(.data$order == 3) %>%
      dplyr::pull(.data$y)
    h <- tb %>%
      dplyr::filter(.data$order == 4) %>%
      dplyr::pull(.data$y)

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

    e <- tb %>%
      dplyr::filter(.data$order == 1) %>%
      dplyr::pull(.data$y)
    f <- tb %>%
      dplyr::filter(.data$order == 2) %>%
      dplyr::pull(.data$y)
    g <- tb %>%
      dplyr::filter(.data$order == 3) %>%
      dplyr::pull(.data$y)
    h <- tb %>%
      dplyr::filter(.data$order == 4) %>%
      dplyr::pull(.data$y)
    i <- tb %>%
      dplyr::filter(.data$order == 5) %>%
      dplyr::pull(.data$y)

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
    dplyr::mutate(
      desc = .data$description,
      desc = str_replace_all(.data$desc, fill, ""),
      desc = str_replace_all(.data$desc, away, ""),
      desc = str_replace_all(.data$desc, goalie, ""),
      desc = str_replace_all(.data$desc, fo, ""),
      # replacing some basic stuff
      on_ice_situation = str_extract(.data$desc, ice),
      desc = str_replace_all(.data$desc, ice, ""),
      # cleaning the on-ice situation
      shot_type = str_extract(.data$desc, shots),
      desc = str_replace_all(.data$desc, shots, ""),
      shot_result = ifelse(str_detect(.data$event, "Goal") & .data$event != "Goalie", "made",
                           str_extract(.data$desc, res)),
      desc = str_replace_all(.data$desc, res, ""),
      # cleaning up shot data to get shot type + the result of the shot
      penalty_type = str_extract(.data$desc, type),
      desc = str_replace_all(.data$desc, type, ""),
      penalty_called = str_extract(.data$desc, pen),
      desc = str_replace_all(.data$desc, pen, ""),
      penalty_length = str_extract(.data$desc,
                                   "[:digit:] mins"),
      desc = str_replace_all(.data$desc,
                             "[:digit:] mins", ""),
      penalty = ifelse(!is.na(.data$penalty_type), 1, 0),
      # cleaning up penalty data
      score = str_extract(.data$desc, score_string),
      # score = ifelse(is.na(score), '0 - 0 T', score),
      # leader = str_extract(score, "[A-Z]+"),
      desc = str_replace_all(.data$desc, score_string, ""),
      desc = str_replace_all(str_trim(.data$desc, side = "both"),"#", ""),
      # cleaning up score data
      first_player = str_trim(str_nth_non_numeric(.data$desc, n = 1)),
      first_number = str_nth_number(.data$desc, n = 1),
      second_player = str_trim(str_nth_non_numeric(.data$desc, n = 2)),
      second_number = str_nth_number(.data$desc, n = 2),
      third_player = str_trim(str_nth_non_numeric(.data$desc, n = 3)),
      third_number = str_nth_number(.data$desc, n = 3)) %>%
    # dplyr::filter(! is.na(time)) %>%
    tidyr::separate(.data$time, into = c("minute", "second"), sep = ":", remove = FALSE) %>%
    dplyr::mutate(
      minute_start = as.numeric(.data$minute),
      second_start = as.numeric(.data$second),
      minute = ifelse(19 - .data$minute_start == 19 &
                        60 - .data$second_start == 60, 20,
                      19 - .data$minute_start),
      second = ifelse(60 - .data$second_start == 60, 0,
                      60 - .data$second_start),
      second = ifelse(.data$second < 10, paste0("0", .data$second),
                      paste0(.data$second)),
      clock = paste0(.data$minute, ":", .data$second)) %>%
    dplyr::select(-.data$minute, -.data$second) %>%
    dplyr::mutate(event_no = dplyr::row_number())

  on_ice <- pbp %>%
    dplyr::filter(is.na(.data$time)) %>%
    dplyr::mutate(event_no = .data$event_no - 1) %>%
    dplyr::select(.data$event, .data$team, .data$event_no, .data$period_id) %>%
    dplyr::mutate(
      team = str_replace_all(team, "#", ""),
      offensive_player_one = str_trim(side = c("both"), str_nth_non_numeric(.data$team, n = 1)),
      offensive_number_one = str_trim(side = c("both"), str_nth_number(.data$team, n = 1)),
      offensive_player_two = str_trim(side = c("both"), str_nth_non_numeric(.data$team, n = 2)),
      offensive_number_two = str_trim(side = c("both"), str_nth_number(.data$team, n = 2)),
      offensive_player_three = str_trim(side = c("both"), str_nth_non_numeric(.data$team, n = 3)),
      offensive_number_three = str_trim(side = c("both"), str_nth_number(.data$team, n = 3)),
      offensive_player_four = str_trim(side = c("both"), str_nth_non_numeric(.data$team, n = 4)),
      offensive_number_four = str_trim(side = c("both"), str_nth_number(.data$team, n = 4)),
      offensive_player_five = str_trim(side = c("both"), str_nth_non_numeric(.data$team, n = 5)),
      offensive_number_five = str_trim(side = c("both"), str_nth_number(.data$team, n = 5))
    ) %>%
    dplyr::select(-c(event, team))

  pbp <- pbp %>%
    dplyr::left_join(on_ice, by = c("period_id", "event_no")) %>%
    dplyr::mutate(
      leader = str_extract(.data$score, "[A-Z]+"),
      scr = str_replace_all(.data$score, "[A-Z]+", "")) %>%
    tidyr::separate(
      .data$scr,
      into = c("away_goals", "home_goals"),
      sep = " - ",
      remove = FALSE) %>%
    dplyr::select(-.data$scr) %>%
    tidyr::fill(.data$score) %>%
    tidyr::fill(.data$leader) %>%
    tidyr::fill(.data$away_goals) %>%
    tidyr::fill(.data$home_goals) %>%
    dplyr::mutate(
      leader = ifelse(is.na(.data$leader), 'T', .data$leader),
      away_goals = ifelse(is.na(.data$away_goals), 0, .data$away_goals),
      home_goals = ifelse(is.na(.data$home_goals), 0, .data$home_goals),
      score = ifelse(is.na(.data$score), '0 - 0 T', .data$score)) %>%
    dplyr::mutate(
      sec_from_start = (60 * .data$minute_start) + .data$second_start,
      sec_from_start = dplyr::case_when(
        .data$period_id == 2 ~ .data$sec_from_start + 1200,
        .data$period_id == 3 ~ .data$sec_from_start + 2400,
        .data$period_id == 4 ~ .data$sec_from_start + 3600,
        .data$period_id == 5 ~ .data$sec_from_start + 4800,
        TRUE ~ .data$sec_from_start),
      power_play_seconds = ifelse(!is.na(.data$penalty_length),
                                  as.numeric(str_extract(.data$penalty_length, '[0-9]')) * 60,
                                  NA_real_),
      start_power_play = ifelse(.data$penalty == 1, .data$sec_from_start, NA_real_),
      end_power_play = ifelse(.data$penalty == 1, .data$start_power_play + .data$power_play_seconds, NA_real_)) %>%
    tidyr::fill(.data$start_power_play) %>%
    tidyr::fill(.data$end_power_play) %>%
    # ID'ing PP situations by whether the timestamp is within the time passed from when the penalty was given
    # any situation that isn't special, i.e. as a PP or Empty Net get replaced by Even Strenght
    dplyr::mutate(
      on_ice_situation = ifelse((.data$sec_from_start >= .data$start_power_play &
                                   .data$sec_from_start <= .data$end_power_play) |
                                  (.data$on_ice_situation == "Power Play"), "Power Play",
                                .data$on_ice_situation),
      on_ice_situation = replace_na(.data$on_ice_situation, "Even Strength"))

  if (nrow(tb) >= 5) {

    shootout <- process_shootout(data = shootout)

    pbp <- dplyr::bind_rows(pbp, shootout)

  }

  pbp <- pbp %>%
    dplyr::left_join(tm, by = character())

  gl <- pbp %>%
    dplyr::filter(.data$event == "Goalie") %>%
    # filter(str_detect(description, "Starting|Returned"))
    dplyr::select(
      .data$home_team, .data$away_team, .data$team, .data$description,
      .data$first_player, .data$event, .data$sec_from_start) %>%
    dplyr::mutate(
      goalie_change = str_extract(.data$description, "Starting|Returned|Pulled"),
      goalie = ifelse(
        str_detect(.data$team, .data$away_team), "away_goalie",
        ifelse(
          str_detect(.data$team, .data$home_team), "home_goalie", NA
        )
      ),
      first_player = ifelse(.data$goalie_change == "Pulled", "None", .data$first_player)
    ) %>%
    dplyr::select(.data$first_player,
                  .data$sec_from_start,
                  .data$goalie_change,
                  .data$goalie) %>%
    tidyr::pivot_wider(
      names_from = .data$goalie,
      values_from = .data$first_player)

  pbp <- pbp %>%
    dplyr::left_join(gl, by = c("sec_from_start")) %>%
    tidyr::fill(.data$home_goalie) %>%
    tidyr::fill(.data$away_goalie) %>%
    dplyr::filter(.data$event != "Goalie") %>%
    dplyr::mutate(
      home_goalie = ifelse(.data$home_goalie == "None", NA_character_, .data$home_goalie),
      away_goalie = ifelse(.data$away_goalie == "None", NA_character_, .data$away_goalie),
      goalie_involved = dplyr::case_when(
        .data$event %in% c("Goal", "PP Goal", "Shot", "Shot BLK") &
          str_detect(.data$team, .data$home_team) ~ .data$away_goalie,
        .data$event %in% c("Goal", "PP Goal", "Shot", "Shot BLK") &
          str_detect(.data$team, .data$away_team) ~ .data$home_goalie,
        TRUE ~ NA_character_),
      time_elapsed = .data$time,
      time_remaining = .data$clock
    )

  return(pbp)

}

#' @title load_pbp
#' @description load_pbp: loads all the play-by-play data for a game into one data frame through just one function
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @import rvest
#' @import tokenizers
#' @import strex
#' @export
#' @examples
#' \dontrun{
#'   first_period <- process_period(data = df[[1]], period = 1)
#' }
load_pbp <- function(game_id = 268078, format = "clean") {

  df <- load_raw_data(game_id = game_id)

  pbp <- pbp_data(data = df)

  pbp <- pbp %>%
    dplyr::filter(!is.na(.data$description)) %>%
    dplyr::mutate(
      game_id = .data$game_id,
      event_no = dplyr::row_number())

  if (format == "clean") {

    pbp <- pbp %>%
      dplyr::select(
        .data$game_id,
        .data$home_team,
        .data$away_team,
        .data$period_id,
        .data$event_no,
        .data$description,
        .data$time_remaining,
        .data$on_ice_situation,
        .data$home_goals, .data$away_goals, .data$leader,
        .data$team,
        .data$event,
        .data$first_player, .data$first_number,
        .data$second_player, .data$second_number,
        .data$third_player, .data$third_number,
        .data$shot_type, .data$shot_result, .data$goalie_involved,
        .data$penalty,
        .data$penalty_length,
        .data$.data$penalty_type,
        .data$penalty_called,
        .data$offensive_player_one,
        .data$offensive_player_two,
        .data$offensive_player_three,
        .data$offensive_player_four,
        .data$offensive_player_five,
        .data$home_goalie,
        .data$away_goalie)

  }

  return(pbp)

}

#### Boxscore Functions ####

boxscore <- data.frame(
  team = character(),
  successful_power_play = numeric(),
  power_play_opportunities = numeric(),
  power_play_percent = numeric(),
  penalty_minutes = numeric(),
  faceoff_percent = numeric(),
  blocked_opponent_shots = numeric(),
  takeaways = numeric(),
  giveaways = numeric(),
  first_shots = integer(),
  second_shots = integer(),
  third_shots = integer(),
  overtime_shots = integer(),
  shootout_shots = integer(),
  total_shots = integer(),
  first_scoring = integer(),
  second_scoring = integer(),
  third_scoring = integer(),
  overtime_scoring = integer(),
  # shootout_scoring = character(),
  total_scoring = integer(),
  winner = character(),
  game_id = numeric()
)

#' @title process_boxscore
#' @description process_boxscore: the code for processing box score data into a format that makes sense
#'
#' @param data the raw data from the game that you're interested in
#' @import tokenizers
#' @import strex
#' @export
#' @examples
#' \dontrun{
#'   boxscore <- process_boxscore(data = df[[1]])
#' }
process_boxscore <- function(data) {

  df <- data[[max(length(data))]]
  score <- data[[max(length(data)) - 2]]
  shot <- data[[max(length(data)) - 1]]

  if (ncol(score) == 5) {

    score <- score %>%
      janitor::clean_names() %>%
      dplyr::rename("team" = "scoring",
             "first_scoring" = "x1st",
             "second_scoring" = "x2nd",
             "third_scoring" = "x3rd",
             "total_scoring" = "t")

  } else if (ncol(score) == 6) {

    score <- score %>%
      janitor::clean_names() %>%
      dplyr::rename("team" = "scoring",
             "first_scoring" = "x1st",
             "second_scoring" = "x2nd",
             "third_scoring" = "x3rd",
             "overtime_scoring" = "ot",
             "total_scoring" = "t")

  } else if (ncol(score) == 7) {

    score <- score %>%
      janitor::clean_names() %>%
      dplyr::rename("team" = "scoring",
             "first_scoring" = "x1st",
             "second_scoring" = "x2nd",
             "third_scoring" = "x3rd",
             "overtime_scoring" = "ot",
             "shootout_scoring" = "so",
             "total_scoring" = "t") %>%
      dplyr::mutate(
        shootout_shots = str_nth_number(shootout_scoring, 3),
        shootout_scoring = str_nth_number(shootout_scoring, 1))

  }

  if (ncol(shot) == 5) {

    shot <- shot %>%
      janitor::clean_names() %>%
      dplyr::rename(
        "team" = "shots",
        "first_shots" = "x1st",
        "second_shots" = "x2nd",
        "third_shots" = "x3rd",
        "total_shots" = "t")

  } else if (ncol(shot) != 5) {

    shot <- shot %>%
      janitor::clean_names() %>%
      dplyr::rename(
        "team" = "shots",
        "first_shots" = "x1st",
        "second_shots" = "x2nd",
        "third_shots" = "x3rd",
        "overtime_shots" = "ot",
        "total_shots" = "t")

  }

  df <- df %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(cols = 2:3) %>%
    tidyr::pivot_wider(names_from = team_stats) %>%
    janitor::clean_names() %>%
    tidyr::separate(
      power_plays,
      into = c("successful_power_play", "power_play_opportunities"),
      sep = " / ") %>%
    dplyr::mutate_at(
      vars(successful_power_play,
           power_play_opportunities,
           power_play_percent,
           penalty_minutes,
           faceoff_percent,
           blocked_opponent_shots,
           takeaways,
           giveaways), as.numeric) %>%
    dplyr::rename("team" = "name")

  s <- shot %>%
    dplyr::left_join(score, by = c("team")) %>%
    dplyr::mutate(team = tolower(team))

  df <- df %>%
    dplyr::left_join(s, by = c("team"))

  df <- dplyr::bind_rows(df, boxscore)

  df <- df %>%
    dplyr::mutate(
      winner = ifelse(
        .data$total_scoring == max(.data$total_scoring, na.rm = TRUE), "Yes", "No"))

  return(df)

}

#' @title load_boxscore
#' @description load_boxscore: loads the boxscore and shot/score data for a game into one data frame through just one function
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @import rvest
#' @import janitor
#' @import httr
#' @import stringr
#' @export
#' @examples
#' \dontrun{
#'   boxscore <- load_boxscore(game_id = 268078)
#' }
load_boxscore <- function(game_id = 268078) {

  y <- game_id

  df <- load_raw_data(game_id = game_id)

  df <- process_boxscore(data = df)

  df <- df %>%
    dplyr::mutate(game_id = y) %>%
    dplyr::select(
      .data$team, .data$game_id, .data$winner, .data$total_scoring,
      .data$first_scoring, .data$second_scoring, .data$third_scoring,
      .data$overtime_scoring, .data$shootout_scoring,
      .data$total_shots, .data$first_shots,
      .data$second_shots, .data$third_shots,
      .data$overtime_shots, .data$shootout_shots,
      .data$blocked_opponent_shots,
      .data$successful_power_play,
      .data$power_play_opportunities,
      .data$power_play_percent,
      .data$faceoff_percent,
      .data$penalty_minutes,
      .data$takeaways,
      .data$giveaways)

  return(df)

}

#' @title load_game
#' @description load_game: loads boxscore and pbp data into a list to load both at once for a given game
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @export
#' @examples
#' \dontrun{
#'   game_data <- load_game(game_id = 268078)
#' }
load_game <- function(game_id = 268078) {

  box <- load_boxscore(game_id = game_id)

  pbp <- load_pbp(game_id = game_id)

  game <- list(box, pbp)

  return(game)

}
