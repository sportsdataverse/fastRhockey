#### updated tags for what gets removed from the text parsing
away <- "[:digit:] GvA|[:digit:] TkA|[:digit:] Blk"
fill <- "from|by|against|to| and|giveaway|Game|Behind|of |Served|served|Bench|bench"
goalie <- "Starting goalie|Pulled goalie|Returned goalie"
fo <- "faceoff won"
ice <- "Even Strength|Empty Net|Power Play|Extra Attacker"
shots <- "Snap shot|Wrist shot|Penalty Shot"
res <- "blocked|saved|failed attempt"
pen <- "Holding|Tripping|Roughing|Hooking|Interference|Delay|Body Checking|Slashing|Check from Behind Misconduct|Checking from Behind|Checking|Ejection|Too Many Men|Delay of Game|Misconduct|Check"
type <- "Minor|Major"
score_string <- "[:digit:] - [:digit:] [A-Z]+|[:digit:] - [:digit:]"
shoot <- "missed attempt against|scores against|Shootout|failed attempt"
lgh <- "[:digit:] mins|[0-9]+ mins"
abbreviations <- "TOR|MIN|BOS|CTW|MET|BUF"
ne <- "On Ice"

#' @title phf_game_data
#' @description phf_game_data: pull in the raw data for a game_id from the PHF/NWHL API
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @import rvest
#' @import httr
#' @import dplyr
#' @importFrom jsonlite parse_json
#' @importFrom purrr pluck
#' @importFrom glue glue
#' @export
#' @examples
#' \dontrun{
#'   phf_game_data(game_id = 368722)
#' }
phf_game_data <- function(game_id = 368719) {
  base_url <- "https://web.api.digitalshift.ca/partials/stats/game/play-by-play?game_id="
  full_url <- paste0(base_url, game_id)

  # setting the ticket as something that can be changed in case the API decides to change it's authorization
  # rather than hard-coding it in
  auth_ticket <- getOption(
    "whockeyR.phf_ticket",
    default = 'ticket="4dM1QOOKk-PQTSZxW_zfXnOgbh80dOGK6eUb_MaSl7nUN0_k4LxLMvZyeaYGXQuLyWBOQhY8Q65k6_uwMu6oojuO"'
  )

  # the link for the game + authorization for accessing the API
  res <- httr::RETRY(
    "GET", full_url,
    httr::add_headers(`Authorization`= auth_ticket))
  # Check the result
  # check_status is defined in the 'utils.R' folder and just checks to make sure that the API actually returns something
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
    # rename some columns so that they are consistent and make sense
    dplyr::rename(
      event = .data$x,
      description = .data$play) %>%
    # only taking events that match to actual on ice stuff
    dplyr::filter(!stringr::str_detect(.data$event, 'On Ice')) %>%
    janitor::remove_empty(which = c("cols"), quiet = TRUE) %>%
    # since the NWHL/PHF website has an 'expansion' tab for goals
    # that gets put into the description column weirdly
    # so essentially filtering that out and replacing it in those cells
    dplyr::mutate(
      description = gsub("{{.*", "", .data$description, perl = TRUE))

  return(data)

}


#' @title load_raw_data
#' @description load_raw_data: pull in the raw data for a game_id from the PHF/NWHL API
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @import rvest
#' @import httr
#' @import purrr
#' @import jsonlite
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
#' @importFrom stringr str_extract str_replace_all str_replace str_detect
#' @importFrom tidyr separate
#' @export
#' @examples
#' \dontrun{
#'   shootout <- process_shootout(data = game_so)
#' }
process_shootout <- function(data) {
  # defining strings that need to be filtered out for shootouts specifically, since they're different than the regular pbp data
  score_string <- "[:digit:] - [:digit:] [A-Z]+|[:digit:] - [:digit:]"
  shoot <- "missed attempt against|scores against|Shootout"
  all <- "[:digit:] - [:digit:] [A-Z]+|[:digit:] - [:digit:]|missed attempt against|scores against|Shootout"

  data <- data %>%
    janitor::clean_names() %>%
    # creating variables, cleaning stuff for shootouts specifically
    # since there's a lot less variation in what can happen
    # it's easier to do the cleaning so it's in its own function
    dplyr::mutate(
      # manually defining some of the event info since it won't change in a shootout situation
      event = "Shootout",
      on_ice_situation = "shootout",
      shot_type = "shootout",
      shot_result = tolower(.data$x),
      period_id = 5,
      # using period = 5 just to keep it numeric and consistent
      event_no = dplyr::row_number(),
      description = .data$play,
      desc = stringr::str_replace_all(.data$play, "#", ""),
      desc = stringr::str_replace_all(.data$desc, shoot, ""),
      score = stringr::str_extract(.data$desc, score_string),
      desc = stringr::str_replace_all(.data$desc, score_string, ""),
      desc = stringr::str_replace_all(str_trim(.data$desc, side = "both"),"#", ""),
      leader = stringr::str_extract(.data$score, "[A-Z]+"),
      scr = stringr::str_replace_all(.data$score, "[A-Z]+", "")) %>%
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
      score = ifelse(is.na(.data$score), '0 - 0 T', .data$score),
      # extracting then replacing player numbers with commas so that we can then separate them to get shooter vs goali
      desc2 = stringr::str_replace_all(.data$description, shoot, ""),
      desc2 = stringr::str_replace_all(.data$desc2, score_string, ""),
      first_number = stringr::str_extract(.data$desc2, "#[0-9]+"),
      desc2 = stringr::str_replace(.data$desc2, first_number, ""),
      second_number = stringr::str_extract(.data$desc2, "#[0-9]+"),
      desc2 = stringr::str_replace(.data$desc2, second_number, ","),
      first_number = stringr::str_trim(stringr::str_replace(.data$first_number, "#", "")),
      second_number = stringr::str_trim(stringr::str_replace(.data$second_number, "#", "")))

  # running separate on the comma separated names to extract player names
  # wrapped in `suppressWarnings()` to prevent it from throwing an error in weird cases about NAs being put in
  suppressWarnings(
    data <- data %>%
      tidyr::separate(desc2, into = c("first_player", "second_player"),
                      sep = ","))

  data <- data %>%
    # trimming off whitespace from player names
    dplyr::mutate(first_player = stringr::str_trim(first_player),
                  second_player = stringr::str_trim(second_player)) %>%
    dplyr::select(-c(desc)) %>%
    # adding an extra line of cleaning in bc things sometimes remained weird
    dplyr::mutate(
      first_player = stringr::str_trim(stringr::str_replace(first_player,
                                                            "missed attempt|scores", "")),
      second_player = stringr::str_trim(stringr::str_replace(second_player,
                                                             "Shootout|Shoout|shoout|shootout", ""))
    )

  return(data)

}

#' @title pbp_data
#' @description pbp_data: returns all of the play-by-play data for a game into on big data frame using the process_period/shootout functions. Contains functionality to account for regulation games, overtime games, and shootouts
#'
#' @param data the raw list data that is generated from the load_raw_data function
#' @importFrom dplyr mutate bind_rows filter row_number select case_when pull starts_with ends_with
#' @importFrom tidyr pivot_wider separate fill replace_na
#' @importFrom stringr str_replace str_replace_all str_extract str_extract_all str_detect str_trim
#' @import rvest
#' @import jsonlite
#' @export
#' @examples \dontrun{
#'   pbp_df <- pbp_data(data = df)
#' }
#### function returning all the pbp data for a game into one big data frame for the game
## data takes the raw list of data from the load_raw_data function
pbp_data <- function(data, game_id = game_id) {

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

  # renaming the game_id variable bc otherwise it doesn't work
  g <- game_id

  # loading in pre-made meta data csv from GitHub bc that's quicker than running a loop through phf_schedule
  tm <- read.csv("https://raw.githubusercontent.com/benhowell71/whockeyR/main/phf_meta_data.csv") %>%
    dplyr::filter(game_id == g) %>%
    dplyr::select(home_team, away_team)

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
      desc = stringr::str_replace_all(.data$desc, fill, ""),
      desc = stringr::str_replace_all(.data$desc, away, ""),
      desc = stringr::str_replace_all(.data$desc, goalie, ""),
      desc = stringr::str_replace_all(.data$desc, fo, ""),
      # replacing some basic stuff
      on_ice_situation = stringr::str_extract(.data$desc, ice),
      desc = stringr::str_replace_all(.data$desc, ice, ""),
      # cleaning the on-ice situation
      shot_type = stringr::str_extract(.data$desc, shots),
      desc = stringr::str_replace_all(.data$desc, shots, ""),
      shot_result = ifelse(stringr::str_detect(.data$event, "Goal") & .data$event != "Goalie", "made",
                           stringr::str_extract(.data$desc, res)),
      desc = stringr::str_replace_all(.data$desc, res, ""),
      # cleaning up shot data to get shot type + the result of the shot
      penalty_type = stringr::str_extract(.data$desc, type),
      desc = stringr::str_replace_all(.data$desc, type, ""),
      penalty_called = stringr::str_extract(.data$desc, pen),
      desc = stringr::str_replace_all(.data$desc, pen, ""),
      penalty_length = stringr::str_extract(.data$desc,
                                   "[:digit:] mins"),
      desc = stringr::str_replace_all(.data$desc,
                             "[:digit:] mins", ""),
      penalty = ifelse(!is.na(.data$penalty_type), 1, 0),
      # cleaning up penalty data
      score = stringr::str_extract(.data$desc, score_string),
      desc = stringr::str_replace_all(.data$desc, score_string, ""),
      desc = stringr::str_replace_all(stringr::str_trim(.data$desc, side = "both"),"#", ""))
      # cleaning up score data
  # wrapping a separate function with suppressWarnings to prevent it from spitting out a 'NA' fill message
  suppressWarnings(pbp <- pbp %>%
    tidyr::separate(.data$time, into = c("minute", "second"),
                    sep = ":", remove = FALSE))

  pbp <- pbp %>%
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
    # extracting player name and number from the description for who is on the ice when a goal was scored
    # the order (player_one vs player_five) doesn't mean anything
    # but with the way str_extract/replace works, we're just pulling the first instance of each number
    # then replacing it with a comma (unless it's the first number bc that doesn't need a comma before that name)
    dplyr::mutate(team = stringr::str_replace_all(team, abbreviations, ""),
                team = stringr::str_replace_all(team, ne, ""),
                number_one = stringr::str_trim(stringr::str_extract(team, "#[0-9]+")),
                team = stringr::str_replace(team, number_one, ""),
                number_two = stringr::str_trim(stringr::str_extract(team, "#[0-9]+")),
                team = stringr::str_replace(team, number_two, ","),
                number_three = stringr::str_trim(stringr::str_extract(team, "#[0-9]+")),
                team = stringr::str_replace(team, number_three, ","),
                number_four = stringr::str_trim(stringr::str_extract(team, "#[0-9]+")),
                team = stringr::str_replace(team, number_four, ","),
                number_five = stringr::str_trim(stringr::str_extract(team, "#[0-9]+")),
                team = stringr::str_replace(team, number_five, ","),
                # there are instances where a team pulls its goalie and has 6 skaters so this is designed to search for that case
                number_six = stringr::str_trim(stringr::str_extract(team, "#[0-9]+")),
                # in the instance where there is NOT a 6th skater, doing a raw str_replace creates a NA and removes the player names
                # so this ifelse statement looks to see if there was a 6th player number and is so, then replace that number with a comma
                # otherwise it just pastes the description there without touching it
                team = ifelse(! is.na(number_six), stringr::str_replace(team, number_six, ","), team))

  suppressWarnings(
    on_ice <- on_ice %>%
    # using the comma separators, separate the string into offensive_player one through six
    tidyr::separate(team, into = c("offensive_player_one", "offensive_player_two",
                            "offensive_player_three", "offensive_player_four",
                            "offensive_player_five", "offensive_player_six"),
             sep = ",", remove = TRUE))

  on_ice <- on_ice %>%
    # trimming the player names to remove whitespace and make them consistent in formatting
    mutate(
      offensive_player_one = stringr::str_trim(offensive_player_one),
      offensive_player_two = stringr::str_trim(offensive_player_two),
      offensive_player_three = stringr::str_trim(offensive_player_three),
      offensive_player_four = stringr::str_trim(offensive_player_four),
      offensive_player_five = stringr::str_trim(offensive_player_five),
      offensive_player_six = stringr::str_trim(offensive_player_six)
    ) %>%
    # de-selecting the unimportant columns
    dplyr::select(-c(event, dplyr::starts_with("number_")))

  pbp <- pbp %>%
    dplyr::left_join(on_ice, by = c("period_id", "event_no")) %>%
    dplyr::mutate(
      leader = stringr::str_extract(.data$score, "[A-Z]+"),
      scr = stringr::str_replace_all(.data$score, "[A-Z]+", "")) %>%
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
      # adding time to the seconds_from_start variable to account for what period we're in
      sec_from_start = dplyr::case_when(
        .data$period_id == 2 ~ .data$sec_from_start + 1200,
        .data$period_id == 3 ~ .data$sec_from_start + 2400,
        .data$period_id == 4 ~ .data$sec_from_start + 3600,
        .data$period_id == 5 ~ .data$sec_from_start + 4800,
        TRUE ~ .data$sec_from_start),
      # who long, in seconds, will the penalty and power play opportunity extend?
      power_play_seconds = ifelse(!is.na(.data$penalty_length),
                                  as.numeric(str_extract(.data$penalty_length, '[0-9]')) * 60,
                                  NA_real_),
      start_power_play = ifelse(.data$penalty == 1, .data$sec_from_start, NA_real_),
      end_power_play = ifelse(.data$penalty == 1, .data$start_power_play + .data$power_play_seconds, NA_real_)) %>%
    tidyr::fill(.data$start_power_play) %>%
    tidyr::fill(.data$end_power_play) %>%
    # ID'ing PP situations by whether the timestamp is within the time passed from when the penalty was given
    # any situation that isn't special, i.e. as a PP or Empty Net get replaced by Even Strength
    dplyr::mutate(
      on_ice_situation = ifelse((.data$sec_from_start >= .data$start_power_play &
                                   .data$sec_from_start <= .data$end_power_play) |
                                  (.data$on_ice_situation == "Power Play"), "Power Play",
                                .data$on_ice_situation),
      on_ice_situation = tidyr::replace_na(.data$on_ice_situation, "Even Strength"))

  if (nrow(tb) >= 5) {
    # adding shootout data to the regulation/OT pbp if there was a shootout
    shootout <- process_shootout(data = shootout)

    pbp <- dplyr::bind_rows(pbp, shootout)

  }

  pbp <- pbp %>%
    dplyr::left_join(tm, by = character())

  pbp <- pbp %>%
    # taking the players and numbers involved in a play
    dplyr::mutate(desc2 = stringr::str_replace_all(description, away, ""),
                  desc2 = stringr::str_replace_all(desc2, fill, ""),
                  desc2 = stringr::str_replace_all(desc2, goalie, ""),
                  desc2 = stringr::str_replace_all(desc2, fo, ""),
                  desc2 = stringr::str_replace_all(desc2, ice, ""),
                  desc2 = stringr::str_replace_all(desc2, shots, ""),
                  desc2 = stringr::str_replace_all(desc2, res, ""),
                  desc2 = stringr::str_replace_all(desc2, pen, ""),
                  desc2 = stringr::str_replace_all(desc2, type, ""),
                  desc2 = stringr::str_replace_all(desc2, shoot, ""),
                  desc2 = stringr::str_replace_all(desc2, score_string, ""),
                  desc2 = stringr::str_replace_all(desc2, lgh, ""),
                  first_number = stringr::str_extract(desc2, "#[0-9]+"),
                  desc2 = stringr::str_replace(desc2, first_number, ""),
                  # don't replace first number with a comma because there is no name in front of the first number
                  second_number = stringr::str_extract(desc2, "#[0-9]+"),
                  # since there isn't always a second or third player involved in a play, using an ifelse statement
                  # to figure out if there was a player, then replacing them if so
                  desc2 = ifelse(! is.na(second_number), stringr::str_replace(desc2, second_number, ","), desc2),
                  third_number = stringr::str_trim(stringr::str_extract(desc2, "#[0-9]+")),
                  desc2 = ifelse(! is.na(third_number), stringr::str_replace(desc2, third_number, ","), desc2),
                  first_number = stringr::str_trim(stringr::str_replace_all(first_number, "#", "")),
                  second_number = stringr::str_trim(stringr::str_replace_all(second_number, "#", "")),
                  third_number = stringr::str_trim(stringr::str_replace_all(third_number, "#", "")))

  # running the player name separation within suppressWarnings to avoid getting 'NA, expected 3 arguments'
  # for plays with just one or two players involved
  suppressWarnings(
    pbp <- pbp %>%
      tidyr::separate(col = desc2, into = c("first_player", "second_player", "third_player"),
                      sep = ",", remove = TRUE))

  # trim whitespace around player names
  pbp <- pbp %>%
    dplyr::mutate(first_player = stringr::str_trim(first_player),
                  second_player = stringr::str_trim(second_player),
                  third_player = stringr::str_trim(third_player))

  gl <- pbp %>%
    dplyr::filter(.data$event == "Goalie") %>%
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
#' @importFrom dplyr mutate bind_rows filter row_number select case_when pull starts_with ends_with
#' @importFrom tidyr pivot_wider separate fill
#' @importFrom stringr str_replace str_replace_all str_extract str_extract_all str_detect str_trim
#' @importFrom tibble rownames_to_column
#' @import rvest
#' @import jsonlite
#' @export
#' @examples
#' \dontrun{
#'   first_period <- process_period(data = df[[1]], period = 1)
#' }
load_pbp <- function(game_id = 268078, format = "clean") {

  # load raw data in from the api
  df <- phf_game_data(game_id = game_id)

  # transform raw data into a pbp dataframe
  pbp <- pbp_data(data = df, game_id = game_id)

  #re-initializing the game_id variable so that it doesn't freak tf out
  x <- game_id

  # some last minute stuff
  pbp <- pbp %>%
    dplyr::filter(!is.na(.data$description)) %>%
    dplyr::mutate(
      game_id = x,
      event_no = dplyr::row_number(),
      power_play_seconds = ifelse(is.na(power_play_seconds), 0,
                                  power_play_seconds))

  # figuring out how many skaters are on the ice at a single time
  away_state_changes <- pbp %>%
    dplyr::filter((event == "PP Goal" & stringr::str_detect(team, home_team)) |
             (event == "Penalty" & stringr::str_detect(team, away_team))) %>%
    dplyr::select(event,sec_from_start,power_play_seconds) %>%
    dplyr::mutate(event = ifelse(event == "Penalty", 1, 2),
           prev.event = lag(event),
           prev.time = lag(sec_from_start),
           prev.length = lag(power_play_seconds))


  away_pen_mat <- apply(away_state_changes,
                        1,
                        FUN = function(x) {
                          #Creates a -1 for duration of penalty and 0s surrounding it
                          if(x[1] == 1 & x[2]+x[3]*60 < (max(pbp$period_id, na.rm = TRUE)*1200-1)){
                            c( rep( 0, length( 0:x[2] )),
                               rep( -1, x[3]*60),
                               rep(0, length((x[2]+x[3]*60 + 1):(max(pbp$period_id, na.rm = TRUE)*1200-1)))
                            )
                            #Creates a -1 for duration of penalty and 0s before (for end of game penalties)
                          } else if(x[1] == 1 & x[2]+x[3]*60 >= (max(pbp$period_id, na.rm = TRUE)*1200-1)) {
                            c( rep( 0, length( 0:x[2] )),
                               rep(-1, max(pbp$period_id, na.rm = TRUE)*1200-1-x[2] )
                            )
                            #Creates a +1 from time power play goal is scored to end of penalty to handle skater coming back on
                          } else if( x[1] == 2 & (x[2] %in% ifelse(!is.na(x[5]) & !is.na(x[6]) & x[2] != x[5], x[5]:(x[5]+x[6]*60),-1 )) ) {
                            c( rep( 0, length( 0:(x[2]) )),
                               rep( 1, length( (x[2]+1):(x[6]*60-(x[2]-x[5])))),
                               rep(0, length((x[6]*60-(x[2]-x[5])):(max(pbp$period_id, na.rm = TRUE)*1200-1)))
                            )
                            # Creates all zeros if event doesnt effect strength
                          } else {
                            rep(0, length(0:(max(pbp$period_id, na.rm = TRUE)*1200-1)))
                          }
                        })

  #creates vector for skaters
  away_skaters <- 5 + apply(away_pen_mat, 1, sum)

  away_skaters <- as.data.frame(away_skaters) %>%
    tibble::rownames_to_column("sec_from_start")%>%
    dplyr::mutate(sec_from_start = as.numeric(sec_from_start))

  home_state_changes <- pbp %>%
    dplyr::filter((event == "PP Goal" & stringr::str_detect(team, away_team)) |
             (event == "Penalty" & stringr::str_detect(team, home_team))) %>%
    dplyr::select(event,sec_from_start,power_play_seconds) %>%
    dplyr::mutate(event = ifelse(event == "Penalty",1,2),
           prev.event = lag(event),
           prev.time = lag(sec_from_start),
           prev.length = lag(power_play_seconds))


  home_pen_mat <- apply(home_state_changes,
                        1,
                        FUN = function(x) {
                          #Creates a -1 for duration of penalty and 0s surrounding it
                          if(x[1] == 1 & (x[2] + x[3] * 60) < (max(pbp$period_id, na.rm = TRUE) * 1200-1)){
                            c( rep( 0, length( 0:x[2] )),
                               rep( -1, x[3]*60),
                               rep(0, length((x[2]+x[3]*60 + 1):(max(pbp$period_id, na.rm = TRUE)*1200-1)))
                            )
                            #Creates a -1 for duration of penalty and 0s before (for end of game penalties)
                          } else if(x[1] == 1 & (x[2]+x[3]*60) >= (max(pbp$period_id, na.rm = TRUE)*1200-1)) {
                            c( rep( 0, length( 0:x[2] )),
                               rep(-1, max(pbp$period_id, na.rm = TRUE)*1200-1-x[2] )
                            )
                            #Creates a +1 from time power play goal is scored to end of penalty to handle skater coming back on
                          } else if( x[1] == 2 & (x[2] %in% ifelse(!is.na(x[5]) & !is.na(x[6]) & x[2] != x[5], x[5]:(x[5]+x[6]*60),-1 )) ) {
                            c( rep( 0, length( 0:(x[2]) )),
                               rep( 1, length( (x[2]+1):(x[6]*60-(x[2]-x[5])))),
                               rep(0, length((x[6]*60-(x[2]-x[5])):(max(pbp$period_id, na.rm = TRUE)*1200-1)))
                            )
                            # Creates all zeros if event doesn't effect strength
                          } else {
                            rep(0, length(0:(max(pbp$period_id)*1200-1)))
                          }
                        })

  #creates vector for skaters
  home_skaters <- 5 + apply(home_pen_mat, 1, sum)

  home_skaters <- as.data.frame(home_skaters) %>%
    tibble::rownames_to_column("sec_from_start")%>%
    dplyr::mutate(sec_from_start = as.numeric(sec_from_start))

  suppressMessages(pbp <- left_join(pbp, home_skaters))
  suppressMessages(pbp <- left_join(pbp, away_skaters))

  pbp <- pbp %>%
    dplyr::mutate(
      first_player = stringr::str_trim(stringr::str_replace(first_player,
                                                            "missed attempt|scores", "")),
      second_player = stringr::str_trim(stringr::str_replace(second_player,
                                                             "Shootout|Shoout|shoout|shootout", ""))
    )

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
        .data$sec_from_start,
        .data$on_ice_situation,
        .data$home_skaters,
        .data$away_skaters,
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
        .data$offensive_player_six,
        .data$home_goalie,
        .data$away_goalie)

  }

  return(pbp)

}

#### Boxscore Functions ####

# create an empty boxscore data frame for binding rows with so that every boxscore has the same size
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
  shootout_scoring = character(),
  total_scoring = integer(),
  winner = character(),
  game_id = numeric()
)

#' @title process_boxscore
#' @description process_boxscore: the code for processing box score data into a format that makes sense
#'
#' @param data the raw data from the game that you're interested in
#' @importFrom dplyr mutate bind_rows filter row_number select case_when pull starts_with ends_with
#' @importFrom tidyr pivot_wider separate fill
#' @importFrom stringr str_replace str_replace_all str_extract str_extract_all str_detect str_trim
#' @importFrom janitor clean_names
#' @import rvest
#' @import jsonlite
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
        shootout_scoring = stringr::str_replace(shootout_scoring, "[0-9] ", ""),
        shootout_scoring = stringr::str_replace(shootout_scoring, "\\(", ""),
        shootout_scoring = stringr::str_replace(shootout_scoring, "\\)", ""),
        shootout_rep = stringr::str_replace(shootout_scoring, " - ", ",")) %>%
      dplyr::select(-c(shootout_scoring)) %>%
      tidyr::separate(shootout_rep, into = c("shootout_scoring", "shootout_shots"),
                      sep = ",", remove = TRUE)

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
#' @import jsonlite
#' @export
#' @examples
#' \dontrun{
#'   boxscore <- load_boxscore(game_id = 268078)
#' }
load_boxscore <- function(game_id = 268078) {

  y <- game_id

  # load raw data from API
  df <- load_raw_data(game_id = game_id)

  # turn raw data into a boxscore format
  df <- process_boxscore(data = df)

  df <- df %>%
    dplyr::mutate(game_id = y) %>%
    dplyr::select(
      .data$team, .data$game_id, .data$winner, .data$total_scoring,
      .data$first_scoring, .data$second_scoring, .data$third_scoring,
      .data$overtime_scoring,
      .data$shootout_scoring,
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
#' @description load_game: loads boxscore/pbp data into a list to load both at once for a given game
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @import rvest
#' @import janitor
#' @import httr
#' @import stringr
#' @import jsonlite
#' @export
#' @examples
#' \dontrun{
#'   game_data <- load_game(game_id = 268078)
#' }
load_game <- function(game_id = 268078) {

  # returns both boxscore and pbp data in a single list
  box <- load_boxscore(game_id = game_id)

  pbp <- load_pbp(game_id = game_id)

  game <- list(box, pbp)

  return(game)

}
