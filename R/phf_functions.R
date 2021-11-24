#### updated tags for what gets removed from the text parsing
away <- "[:digit:] GvA|[:digit:] TkA|[:digit:] Blk"
fill <- "from| by|against|to| and|giveaway|Game|Behind|of |Served|served|Bench|bench"
goalie <- "Starting goalie|Pulled goalie|Returned goalie"
fo <- "faceoff won"
ice <- "Even Strength|Empty Net|Power Play|Extra Attacker|Short Handed"
shots <- "Snap shot|Wrist shot|Penalty Shot"
res <- "blocked|saved|failed attempt"
pen <- "Holding the Stick|Holding|Tripping|Roughing|Hooking|Interference|Diving|Delay|Cross-Checking|Head Contact|Body Checking|Slashing|Check from Behind Misconduct|Checking from Behind|Checking|Ejection|Too Many Men|Delay of Game|Misconduct|Check|High-Sticking"
type <- "Minor|Major"
score_string <- "[:digit:] - [:digit:] [A-Z]+|[:digit:] - [:digit:]"
shoot <- "missed attempt against|scores against|Shootout|failed attempt"
lgh <- "[:digit:] mins|[0-9]+ mins"
abbreviations <- "TOR|MIN|BOS|CTW|MET|BUF"
ne <- "On Ice"







#' @title phf_pbp_data
#' @description phf_pbp_data: returns all of the play-by-play data for a game into on big data frame using the process_phf_period/shootout functions. Contains functionality to account for regulation games, overtime games, and shootouts
#'
#' @param data the raw list data that is generated from the phf_game_raw function
#' @param game_id the game ID of the game that you want pbp data processed for
#' @importFrom dplyr mutate bind_rows filter row_number select case_when pull starts_with ends_with
#' @importFrom tidyr pivot_wider separate fill replace_na
#' @importFrom stringr str_replace str_replace_all str_extract str_extract_all str_detect str_trim
#' @importFrom utils read.csv
#' @import rvest
#' @import jsonlite
#' @export
#### function returning all the pbp data for a game into one big data frame for the game
## data takes the raw list of data from the phf_game_raw function
phf_pbp_data <- function(data, game_id = game_id) {

  lst <- list()
  # creating an empty list

  # so, since there's not a consistent format of which table in the list the period data is in
  # I have to have it loop through the number of rows in each of those tables
  # then we take each one that has at least 6 observations
  # for (y in 1:length(data)) {
  #
  #   z <- ncol(data[[y]])
  #   tb <- data.frame(y, z)
  #
  #   lst[[y]] <- tb
  #
  # }
  # # 6 observations bc the shootout format is 3 shots per team at minimum
  # # since there are 7 lines in one of the boxscore tabs, we have to be careful
  # # however, that is always one of the last tables so we can just take the first five
  # tb <- dplyr::bind_rows(lst)
  #   # LOOK AT
  #   # z > 7 runs easy, but it's possible for a shootout to be only 6 or 7 events
  #   # and since there's another table that is 7 (boxscore data) it causes errors when I left it
  #   # as >= 6 so I changed it for now to work
  #   dplyr::filter(.data$z > 7) %>%
  #   dplyr::mutate(order = dplyr::row_number()) %>%
  #   dplyr::filter(.data$order > 0, .data$order < 6)

  # renaming the game_id variable bc otherwise it doesn't work
  g <- game_id

  # loading in pre-made meta data csv from GitHub bc that's quicker than running a loop through phf_schedule
  tm <- read.csv("https://raw.githubusercontent.com/benhowell71/fastRhockey/main/data-raw/raw_data/phf_game_meta.csv") %>%
    dplyr::filter(game_id == g) %>%
    dplyr::select(.data$home_team, .data$away_team)

  # ss <- list()
  #
  # for (y in 2016:2022) {
  #
  #   season <- phf_schedule(season = 2022)
  #
  #   ss[[y]] <- season
  #
  # }
  #
  # s <- dplyr::bind_rows(ss)
  #
  # tm <- s %>%
  #   dplyr::filter(game_id == g) %>%
  #   dplyr::select(.data$home_team, .data$away_team)

  # creating the pbp dataframes for regulation, OT, or shootout games
  if (length(data) == 3) {

    # e <- tb %>%
    #   dplyr::filter(.data$order == 1) %>%
    #   dplyr::pull(.data$y)
    # f <- tb %>%
    #   dplyr::filter(.data$order == 2) %>%
    #   dplyr::pull(.data$y)
    # g <- tb %>%
    #   dplyr::filter(.data$order == 3) %>%
    #   dplyr::pull(.data$y)

    first_period <- data[[1]]
    second_period <- data[[2]]
    third_period <- data[[3]]

    first_period <- process_phf_period(data = first_period, period = 1)

    second_period <- process_phf_period(data = second_period, period = 2)

    third_period <- process_phf_period(data = third_period, period = 3)

    pbp <- dplyr::bind_rows(first_period,
                            second_period,
                            third_period)

  } else if (length(data) == 4) {
    #
    #     e <- tb %>%
    #       dplyr::filter(.data$order == 1) %>%
    #       dplyr::pull(.data$y)
    #     f <- tb %>%
    #       dplyr::filter(.data$order == 2) %>%
    #       dplyr::pull(.data$y)
    #     g <- tb %>%
    #       dplyr::filter(.data$order == 3) %>%
    #       dplyr::pull(.data$y)
    #     h <- tb %>%
    #       dplyr::filter(.data$order == 4) %>%
    #       dplyr::pull(.data$y)

    first_period <- data[[1]]
    second_period <- data[[2]]
    third_period <- data[[3]]
    fourth_period <- data[[4]]

    first_period <- process_phf_period(data = first_period, period = 1)

    second_period <- process_phf_period(data = second_period, period = 2)

    third_period <- process_phf_period(data = third_period, period = 3)

    fourth_period <- process_phf_period(data = fourth_period, period = 4)

    pbp <- dplyr::bind_rows(first_period,
                            second_period,
                            third_period,
                            fourth_period)

  } else if (length(data) == 5) {

    # e <- tb %>%
    #   dplyr::filter(.data$order == 1) %>%
    #   dplyr::pull(.data$y)
    # f <- tb %>%
    #   dplyr::filter(.data$order == 2) %>%
    #   dplyr::pull(.data$y)
    # g <- tb %>%
    #   dplyr::filter(.data$order == 3) %>%
    #   dplyr::pull(.data$y)
    # h <- tb %>%
    #   dplyr::filter(.data$order == 4) %>%
    #   dplyr::pull(.data$y)
    # i <- tb %>%
    #   dplyr::filter(.data$order == 5) %>%
    #   dplyr::pull(.data$y)

    first_period <- data[[1]]
    second_period <- data[[2]]
    third_period <- data[[3]]
    fourth_period <- data[[4]]
    shootout <- data[[5]]

    first_period <- process_phf_period(data = first_period, period = 1)

    second_period <- process_phf_period(data = second_period, period = 2)

    third_period <- process_phf_period(data = third_period, period = 3)

    fourth_period <- process_phf_period(data = fourth_period, period = 4)

    pbp <- dplyr::bind_rows(first_period,
                            second_period,
                            third_period,
                            fourth_period)

  } else if (length(data) >= 6) {

    first_period <- data[[1]]
    second_period <- data[[2]]
    third_period <- data[[3]]
    fourth_period <- data[[4]]
    shootout <- data[[6]]

    first_period <- process_phf_period(data = first_period, period = 1)

    second_period <- process_phf_period(data = second_period, period = 2)

    third_period <- process_phf_period(data = third_period, period = 3)

    fourth_period <- process_phf_period(data = fourth_period, period = 4)

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
    dplyr::mutate(
      team = stringr::str_replace_all(.data$team, abbreviations, ""),
      team = stringr::str_replace_all(.data$team, ne, ""),
      number_one = stringr::str_trim(stringr::str_extract(.data$team, "#[0-9]+")),
      team = stringr::str_replace(.data$team, .data$number_one, ""),
      number_two = stringr::str_trim(stringr::str_extract(.data$team, "#[0-9]+")),
      team = stringr::str_replace(.data$team, .data$number_two, ","),
      number_three = stringr::str_trim(stringr::str_extract(.data$team, "#[0-9]+")),
      team = stringr::str_replace(.data$team, .data$number_three, ","),
      number_four = stringr::str_trim(stringr::str_extract(.data$team, "#[0-9]+")),
      team = stringr::str_replace(.data$team, .data$number_four, ","),
      number_five = stringr::str_trim(stringr::str_extract(.data$team, "#[0-9]+")),
      team = stringr::str_replace(.data$team, .data$number_five, ","),
      # there are instances where a team pulls its goalie and has 6 skaters so this is designed to search for that case
      number_six = stringr::str_trim(stringr::str_extract(.data$team, "#[0-9]+")),
      # in the instance where there is NOT a 6th skater, doing a raw str_replace creates a NA and removes the player names
      # so this ifelse statement looks to see if there was a 6th player number and is so, then replace that number with a comma
      # otherwise it just pastes the description there without touching it
      team = ifelse(! is.na(.data$number_six),
                    stringr::str_replace(.data$team, .data$number_six, ","), .data$team))

  suppressWarnings(
    on_ice <- on_ice %>%
      # using the comma separators, separate the string into offensive_player one through six
      tidyr::separate(.data$team, into = c("offensive_player_one", "offensive_player_two",
                                           "offensive_player_three", "offensive_player_four",
                                           "offensive_player_five", "offensive_player_six"),
                      sep = ",", remove = TRUE))

  on_ice <- on_ice %>%
    # trimming the player names to remove whitespace and make them consistent in formatting
    dplyr::mutate(
      offensive_player_one = stringr::str_trim(.data$offensive_player_one),
      offensive_player_two = stringr::str_trim(.data$offensive_player_two),
      offensive_player_three = stringr::str_trim(.data$offensive_player_three),
      offensive_player_four = stringr::str_trim(.data$offensive_player_four),
      offensive_player_five = stringr::str_trim(.data$offensive_player_five),
      offensive_player_six = stringr::str_trim(.data$offensive_player_six)) %>%
    # de-selecting the unimportant columns
    dplyr::select(-c(.data$event, dplyr::starts_with("number_")))

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

  if (length(data) >= 5) {
    # adding shootout data to the regulation/OT pbp if there was a shootout
    shootout <- process_phf_shootout(data = shootout)

    pbp <- dplyr::bind_rows(pbp, shootout)

  }

  pbp <- pbp %>%
    dplyr::left_join(tm, by = character())

  pbp <- pbp %>%
    # taking the players and numbers involved in a play
    dplyr::mutate(
      desc2 = stringr::str_replace_all(.data$description, away, ""),
      desc2 = stringr::str_replace_all(.data$desc2, fill, ""),
      desc2 = stringr::str_replace_all(.data$desc2, goalie, ""),
      desc2 = stringr::str_replace_all(.data$desc2, fo, ""),
      desc2 = stringr::str_replace_all(.data$desc2, ice, ""),
      desc2 = stringr::str_replace_all(.data$desc2, shots, ""),
      desc2 = stringr::str_replace_all(.data$desc2, res, ""),
      desc2 = stringr::str_replace_all(.data$desc2, pen, ""),
      desc2 = stringr::str_replace_all(.data$desc2, type, ""),
      desc2 = stringr::str_replace_all(.data$desc2, shoot, ""),
      desc2 = stringr::str_replace_all(.data$desc2, score_string, ""),
      desc2 = stringr::str_replace_all(.data$desc2, lgh, ""),
      first_number = stringr::str_extract(.data$desc2, "#[0-9]+"),
      desc2 = stringr::str_replace(.data$desc2, .data$first_number, ""),
      # don't replace first number with a comma because there is no name in front of the first number
      second_number = stringr::str_extract(.data$desc2, "#[0-9]+"),
      # since there isn't always a second or third player involved in a play, using an ifelse statement
      # to figure out if there was a player, then replacing them if so
      desc2 = ifelse(! is.na(.data$second_number),
                     stringr::str_replace(.data$desc2, .data$second_number, ","), .data$desc2),
      third_number = stringr::str_trim(stringr::str_extract(.data$desc2, "#[0-9]+")),
      desc2 = ifelse(! is.na(.data$third_number),
                     stringr::str_replace(.data$desc2, .data$third_number, ","), .data$desc2),
      first_number = stringr::str_trim(stringr::str_replace_all(.data$first_number, "#", "")),
      second_number = stringr::str_trim(stringr::str_replace_all(.data$second_number, "#", "")),
      third_number = stringr::str_trim(stringr::str_replace_all(.data$third_number, "#", "")))

  # running the player name separation within suppressWarnings to avoid getting 'NA, expected 3 arguments'
  # for plays with just one or two players involved
  suppressWarnings(
    pbp <- pbp %>%
      tidyr::separate(col = .data$desc2, into = c("first_player", "second_player", "third_player"),
                      sep = ",", remove = TRUE))

  # trim whitespace around player names
  pbp <- pbp %>%
    dplyr::mutate(
      first_player = stringr::str_trim(.data$first_player),
      second_player = stringr::str_trim(.data$second_player),
      third_player = stringr::str_trim(.data$third_player))

  gl <- pbp %>%
    dplyr::filter(.data$event == "Goalie") %>%
    dplyr::select(
      .data$home_team, .data$away_team, .data$team, .data$description,
      .data$first_player, .data$event, .data$sec_from_start) %>%
    dplyr::mutate(
      goalie_change = stringr::str_extract(.data$description, "Starting|Returned|Pulled"),
      goalie = ifelse(
        stringr::str_detect(.data$team, .data$away_team), "away_goalie",
        ifelse(
          stringr::str_detect(.data$team, .data$home_team), "home_goalie", NA
        )
      ),
      first_player = ifelse(.data$goalie_change == "Pulled", "None", .data$first_player)) %>%
    dplyr::select(
      .data$first_player,
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
      time_remaining = .data$clock)

  return(pbp)

}

#' @title load_phf_pbp
#' @description load_phf_pbp: loads all the play-by-play data for a game into one data frame through just one function
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @param format Whether the data should be returned in a clean format or the messy data file that doesn't filter out columns
#' @importFrom dplyr mutate bind_rows filter row_number select case_when pull starts_with ends_with
#' @importFrom tidyr pivot_wider separate fill
#' @importFrom stringr str_replace str_replace_all str_extract str_extract_all str_detect str_trim
#' @importFrom tibble rownames_to_column
#' @importFrom utils read.csv
#' @importFrom glue glue
#' @import rvest
#' @import jsonlite
#' @export
load_phf_pbp <- function(game_id = 268078, format = "clean") {

  tryCatch(
    expr = {
      # load raw data in from the api
      df <- phf_pbp(game_id = game_id)
      g <- game_id

      # loading in pre-made meta data csv from GitHub bc that's quicker than running a loop through phf_schedule
      tm <- read.csv("https://raw.githubusercontent.com/benhowell71/fastRhockey/main/data-raw/raw_data/phf_game_meta.csv") %>%
        dplyr::filter(game_id == g) %>%
        dplyr::select(.data$home_team, .data$away_team)
      # transform raw data into a pbp dataframe
      pbp <- phf_pbp_data(data = df, game_id = game_id)

      #re-initializing the game_id variable so that it doesn't freak tf out
      x <- game_id

      # some last minute stuff
      pbp <- pbp %>%
        dplyr::filter(! is.na(.data$description)) %>%
        dplyr::mutate(
          game_id = x,
          event_no = dplyr::row_number(),
          power_play_seconds = ifelse(is.na(.data$power_play_seconds), 0,
                                      .data$power_play_seconds))

      # figuring out how many skaters are on the ice at a single time
      away_state_changes <- pbp %>%
        dplyr::filter((.data$event == "PP Goal" & stringr::str_detect(.data$team, .data$home_team)) |
                        (.data$event == "Penalty" & stringr::str_detect(.data$team, .data$away_team))) %>%
        dplyr::select(.data$event, .data$sec_from_start, .data$power_play_seconds) %>%
        dplyr::mutate(event = ifelse(.data$event == "Penalty", 1, 2),
                      prev.event = lag(.data$event),
                      prev.time = lag(.data$sec_from_start),
                      prev.length = lag(.data$power_play_seconds))

      away_pen_mat <- apply(away_state_changes,
                            1,
                            FUN = function(x) {
                              #Creates a -1 for duration of penalty and 0s surrounding it
                              if(x[1] == 1 & x[2]+x[3] < (max(pbp$period_id, na.rm = TRUE)*1200-1)){
                                c( rep( 0, length( 0:x[2] )),
                                   rep( -1, x[3]),
                                   rep(0, length((x[2]+x[3] + 1):(max(pbp$period_id, na.rm = TRUE)*1200-1)))
                                )
                                #Creates a -1 for duration of penalty and 0s before (for end of game penalties)
                              } else if (x[1] == 1 & (x[2] == (max(pbp$period_id, na.rm = TRUE)*1200))) {
                                c( rep( 0, length( 0:(max(pbp$period_id, na.rm = TRUE)*1200-1) )))
                              } else if(x[1] == 1 & x[2]+x[3] >= (max(pbp$period_id, na.rm = TRUE)*1200-1)) {
                                c( rep( 0, length( 0:x[2] )),
                                   rep(-1, max(pbp$period_id, na.rm = TRUE)*1200-1-x[2] )
                                )
                                #Creates a +1 from time power play goal is scored to end of penalty to handle skater coming back on
                              } else if( x[1] == 2 & (x[2] %in% ifelse(!is.na(x[5]) & !is.na(x[6]) & x[2] != x[5], x[5]:(x[5]+x[6]),-1 )) ) {
                                c( rep( 0, length( 0:(x[2]) )),
                                   rep( 1, length( (x[2]+1):(x[6]-(x[2]-x[5])))),
                                   rep(0, length((x[6]-(x[2]-x[5])):(max(pbp$period_id, na.rm = TRUE)*1200-1)))
                                )
                                # Creates all zeros if event doesnt effect strength
                              } else {
                                rep(0, length(0:(max(pbp$period_id, na.rm = TRUE)*1200-1)))
                              }
                            })

      if (typeof(away_pen_mat) == "list") {

        away_pen_mat <- matrix(unlist(away_pen_mat), ncol = nrow(away_state_changes), byrow = TRUE)

      }

      adim <- dim(away_pen_mat)

      #creates vector for skaters
      if (! is.null(adim)) {

        away_skaters <- 5 + apply(away_pen_mat, 1, sum)

        away_skaters <- as.data.frame(away_skaters) %>%
          tibble::rownames_to_column("sec_from_start")%>%
          dplyr::mutate(sec_from_start = as.numeric(.data$sec_from_start))

      } else if (is.null(adim)) {

        sec <- seq(1, max(pbp$period_id) * 1200)
        a_skate <- 5

        away_skaters <- data.frame(sec, a_skate)

        colnames(away_skaters) <- c("sec_from_start", "away_skaters")

      }

      home_state_changes <- pbp %>%
        dplyr::filter((.data$event == "PP Goal" & stringr::str_detect(.data$team, .data$away_team)) |
                        (.data$event == "Penalty" & stringr::str_detect(.data$team, .data$home_team))) %>%
        dplyr::select(.data$event, .data$sec_from_start, .data$power_play_seconds) %>%
        dplyr::mutate(
          event = ifelse(.data$event == "Penalty",1,2),
                      prev.event = lag(.data$event),
                      prev.time = lag(.data$sec_from_start),
                      prev.length = lag(.data$power_play_seconds))

      home_pen_mat <- apply(home_state_changes,
                            1,
                            FUN = function(x) {
                              #Creates a -1 for duration of penalty and 0s surrounding it
                              if(x[1] == 1 & (x[2] + x[3]) < (max(pbp$period_id, na.rm = TRUE) * 1200-1)){
                                c( rep( 0, length( 0:x[2] )),
                                   rep( -1, x[3]),
                                   rep(0, length((x[2]+x[3] + 1):(max(pbp$period_id, na.rm = TRUE)*1200-1)))
                                )
                                #Creates a -1 for duration of penalty and 0s before (for end of game penalties)
                              } else if (x[1] == 1 & (x[2] == (max(pbp$period_id, na.rm = TRUE)*1200))) {
                                c( rep( 0, length( 0:(max(pbp$period_id, na.rm = TRUE)*1200-1) )))
                              } else if(x[1] == 1 & (x[2]+x[3]) >= (max(pbp$period_id, na.rm = TRUE)*1200-1)) {
                                c( rep( 0, length( 0:x[2] )),
                                   rep(-1, max(pbp$period_id, na.rm = TRUE)*1200-1-x[2] )
                                )
                                #Creates a +1 from time power play goal is scored to end of penalty to handle skater coming back on
                              } else if( x[1] == 2 & (x[2] %in% ifelse(!is.na(x[5]) & !is.na(x[6]) & x[2] != x[5], x[5]:(x[5]+x[6]),-1 )) ) {
                                c( rep( 0, length( 0:(x[2]) )),
                                   rep( 1, length( (x[2]+1):(x[6]-(x[2]-x[5])))),
                                   rep(0, length((x[6]-(x[2]-x[5])):(max(pbp$period_id, na.rm = TRUE)*1200-1)))
                                )
                                # Creates all zeros if event doesn't effect strength
                              } else {
                                rep(0, length(0:(max(pbp$period_id)*1200-1)))
                              }
                            })

      if (typeof(home_pen_mat) == "list") {

        home_pen_mat <- matrix(unlist(home_pen_mat), ncol = nrow(home_state_changes), byrow = TRUE)

      }

      hdim <- dim(home_pen_mat)

      #creates vector for skaters
      if (! is.null(adim)) {

        home_skaters <- 5 + apply(home_pen_mat, 1, sum)

        home_skaters <- as.data.frame(home_skaters) %>%
          tibble::rownames_to_column("sec_from_start")%>%
          dplyr::mutate(sec_from_start = as.numeric(.data$sec_from_start))

      } else if (is.null(adim)) {

        hsec <- seq(1, max(pbp$period_id) * 1200)
        h_skate <- 5

        home_skaters <- data.frame(hsec, h_skate)

        colnames(home_skaters) <- c("sec_from_start", "home_skaters")

      }
      #
      #   away_state_changes <- away_state_changes %>%
      #     dplyr::mutate(change_skaters = ifelse(event == 1, -1,
      #                                    ifelse(event == 2, 1, NA)))
      #
      #   home_state_changes <- home_state_changes %>%
      #     dplyr::mutate(change_skaters = ifelse(event == 1, -1,
      #                                    ifelse(event == 2, 1, NA)))
      #
      #   away_skaters <- away_skaters  %>%
      #     dplyr::mutate(normal_skaters = 5)
      #
      #   away_skaters %>%
      #     left_join(away_state_changes %>%
      #                 dplyr::select(sec_from_start, power_play_seconds,
      #                               change_skaters) %>%
      #                 dplyr::mutate(end_power_play = sec_from_start + power_play_seconds)) %>%
      #     tidyr::fill(end_power_play) %>%
      #     # tidyr::fill(change_skaters) %>%
      #     dplyr::mutate(skaters = ifelse(sec_from_start <= end_power_play,
      #                                    normal_skaters + change_skaters, normal_skaters),
      #                   skaters = ifelse(sec_from_start <= end_power_play, lag(skaters), skaters)) -> f
      #
      #   home_skaters <- home_skaters %>%
      #     dplyr::mutate(normal_skaters = 5)

      suppressMessages(pbp <- left_join(pbp, home_skaters))
      suppressMessages(pbp <- left_join(pbp, away_skaters))

      pbp <- pbp %>%
        dplyr::mutate(
          first_player = stringr::str_trim(stringr::str_replace(.data$first_player,
                                                                "missed attempt|scores", "")),
          second_player = stringr::str_trim(stringr::str_replace(.data$second_player,
                                                                 "Shootout|Shoout|shoout|shootout", ""))
        )

      # accounts for pulled goalie
      # and deals with more than two penalties at one time for a single team
      pbp <- pbp %>%
        dplyr::mutate(
          home_skaters = ifelse(.data$home_skaters < 3, 3, .data$home_skaters),
          away_skaters = ifelse(.data$away_skaters < 3, 3, .data$away_skaters),
          home_skaters = ifelse(is.na(.data$home_goalie), .data$home_skaters + 1, .data$home_skaters),
          away_skaters = ifelse(is.na(.data$away_goalie), .data$away_skaters + 1, .data$away_skaters),
          home_skaters = ifelse(.data$sec_from_start == 0, 5, .data$home_skaters),
          away_skaters = ifelse(.data$sec_from_start == 0, 5, .data$away_skaters),
          on_ice_situation = ifelse(.data$home_skaters != .data$away_skaters,
                                    "Power Play", "Even Strength")
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

    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid game_id or no data available! Try using `phf_schedule` to find a game ID to look up!"))
    },
    warning = function(w) {

    },
    finally = {

    }
  )

  return(pbp)

}


