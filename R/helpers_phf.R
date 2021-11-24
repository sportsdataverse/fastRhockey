#' @title helper_phf_pbp_normalize_columns
#' @description First in processing pipeline to give normalized columns
#'
#' @param df play-by-play data frame
#' @return A data-frame with the following columns:
#' * play_type
#' * team
#' * time
#' * play_description
#' * scoring_team_abbrev
#' * scoring_team_on_ice
#' * defending_team_abbrev
#' * defending_team_on_ice
#' @importFrom dplyr mutate mutate_at bind_cols lead filter select
#' @importFrom stringr str_detect
#'
helper_phf_pbp_normalize_columns <- function(df){
  if(ncol(df)==3){
    colnames(df) <- c("play_type","team","play_description")
    df$time <- NA_character_
    df <- df %>%
      dplyr::select(.data$play_type,.data$team, .data$time, .data$play_description)
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
helper_phf_pbp_data <- function(data, game_id = game_id) {

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

  pbp <- pbp %>%
    # replacing extraneous words to parse out player names
    dplyr::mutate(
      desc = .data$play_description,
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
      shot_result = ifelse(stringr::str_detect(.data$play_type, "Goal") & .data$play_type != "Goalie", "made",
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
    dplyr::mutate(play_no = dplyr::row_number())

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

#' @title helper_phf_team_box
#' @description helper_phf_team_box: the code for processing box score data into a format that makes sense
#'
#' @param data the raw data from the game that you're interested in
#' @importFrom dplyr mutate bind_rows filter row_number select case_when pull starts_with ends_with
#' @importFrom tidyr pivot_wider separate fill
#' @importFrom stringr str_replace str_replace_all str_extract str_extract_all str_detect str_trim
#' @importFrom janitor clean_names
#' @import rvest
#' @import jsonlite
#' @export
#' @examples \donttest{
#'   df <- phf_game_raw(game_id = 268078)
#'   boxscore <- helper_phf_team_box(data = df)
#' }
helper_phf_team_box <- function(data) {

  df <- data[[max(length(data))]]
  score <- data[[max(length(data)) - 2]]
  shot <- data[[max(length(data)) - 1]]

  start_names <- c("team", "period_1_scoring", "total_scoring")
  in_prog_names <- c("team","period_1_scoring","period_2_scoring", "total_scoring")
  reg_names <- c("team","period_1_scoring","period_2_scoring",
                 "period_3_scoring", "total_scoring")
  ot_only_names <- c("team","period_1_scoring","period_2_scoring",
                     "period_3_scoring", "overtime_scoring",
                     "total_scoring")
  shootout_scoring_init_names <- c("team","period_1_scoring","period_2_scoring",
                      "period_3_scoring", "overtime_scoring",
                      "shootout_made_scoring",  "total_scoring", "shootout_missed_scoring")
  shootout_names <- c("team","period_1_scoring","period_2_scoring",
                      "period_3_scoring", "overtime_scoring",
                      "shootout_made_scoring", "shootout_missed_scoring", "total_scoring")
  shootout_shot_init_names <- c("team","period_1_shots","period_2_shots",
                                "period_3_shots", "overtime_shots", "total_shots",
                                "shootout_made_shots","shootout_missed_shots")
  shootout_shot_names <- c("team","period_1_shots","period_2_shots",
                           "period_3_shots", "overtime_shots",
                           "shootout_made_shots","shootout_missed_shots", "total_shots")


  if(ncol(score) == 3) {

    colnames(score) <- start_names
    score$period_2_scoring <- NA_integer_
    score$period_3_scoring <- NA_integer_
    score$overtime_scoring <- NA_integer_
    score$shootout_made_scoring <- NA_integer_
    score$shootout_missed_scoring <- NA_integer_

    score <- score %>%
      dplyr::select(dplyr::all_of(shootout_names))

    shot$period_2_scoring <- NA_integer_
    shot$period_3_scoring <- NA_integer_
    shot$overtime_scoring <- NA_integer_
    shot$shootout_made_scoring <- NA_integer_
    shot$shootout_missed_scoring <- NA_integer_
    colnames(shot) <- gsub("_scoring", "_shots", colnames(score))
    shot <- shot %>%
      dplyr::select(dplyr::all_of(shootout_shot_names))

  } else if (ncol(score) == 4) {

    colnames(score) <- in_prog_names
    score$period_3_scoring <- NA_integer_
    score$overtime_scoring <- NA_integer_
    score$shootout_made_scoring <- NA_integer_
    score$shootout_missed_scoring <- NA_integer_
    score <- score %>%
      dplyr::select(dplyr::all_of(shootout_names))

    shot$period_3_scoring <- NA_integer_
    shot$overtime_scoring <- NA_integer_
    shot$shootout_made_scoring <- NA_integer_
    shot$shootout_missed_scoring <- NA_integer_
    colnames(shot) <- gsub("_scoring", "_shots", colnames(score))
    shot <- shot  %>%
      dplyr::select(dplyr::all_of(shootout_shot_names))

  } else if (ncol(score) == 5) {

    colnames(score) <- reg_names
    score$overtime_scoring <- NA_integer_
    score$shootout_made_scoring <- NA_integer_
    score$shootout_missed_scoring <- NA_integer_
    score <- score %>%
      dplyr::select(dplyr::all_of(shootout_names))

    colnames(shot) <- gsub("_scoring", "_shots",reg_names)
    shot$overtime_shots <- NA_integer_
    shot$shootout_made_shots <- NA_integer_
    shot$shootout_missed_shots <- NA_integer_
    shot <- shot %>%
      dplyr::select(dplyr::all_of(shootout_shot_names))

  } else if (ncol(score) == 6) {

    colnames(score) <- ot_only_names
    score$shootout_made_scoring <- NA_integer_
    score$shootout_missed_scoring <- NA_integer_
    score <- score %>%
      dplyr::select(dplyr::all_of(shootout_names))
    colnames(shot) <- gsub("_scoring", "_shots", ot_only_names)
    shot$shootout_made_shots <- NA_integer_
    shot$shootout_missed_shots <- NA_integer_
    colnames(shot) <- shootout_shot_init_names
    shot <- shot %>%
      dplyr::select(dplyr::all_of(shootout_shot_names))
    colnames(shot) <- gsub("_scoring", "_shots", colnames(score))
    shot <- shot %>%
      dplyr::select(dplyr::all_of(shootout_shot_names))

  } else if (ncol(score) == 7) {

    score$shootout_missed_scoring <- NA_integer_
    colnames(score) <- shootout_scoring_init_names
    score <- score %>%
      dplyr::select(dplyr::all_of(shootout_names))
    shot$shootout_made_scoring <- NA_integer_
    shot$shootout_missed_scoring <- NA_integer_
    colnames(shot) <- shootout_shot_init_names
    shot <- shot %>%
      dplyr::select(shootout_shot_names)

    score <- score %>%
      dplyr::mutate(
        shootout_made_scoring = stringr::str_replace(.data$shootout_made_scoring, "[0-9] ", ""),
        shootout_made_scoring = stringr::str_replace(.data$shootout_made_scoring, "\\(", ""),
        shootout_made_scoring = stringr::str_replace(.data$shootout_made_scoring, "\\)", ""),
        shootout_rep = stringr::str_replace(.data$shootout_made_scoring, " - ", ",")) %>%
      dplyr::select(-c(.data$shootout_made_scoring)) %>%
      tidyr::separate(.data$shootout_rep, into = c("shootout_made_scoring", "shootout_missed_scoring"),
                      sep = ",", remove = TRUE) %>%
      dplyr::mutate(shootout_missed_scoring = as.numeric(.data$shootout_missed_scoring))

  }

  df <- df %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(cols = 2:3) %>%
    tidyr::pivot_wider(names_from = .data$team_stats) %>%
    janitor::clean_names() %>%
    tidyr::separate(
      .data$power_plays,
      into = c("successful_power_play", "power_play_opportunities"),
      sep = " / ") %>%
    dplyr::mutate_at(
      vars(.data$successful_power_play,
           .data$power_play_opportunities,
           .data$power_play_percent,
           .data$penalty_minutes,
           .data$faceoff_percent,
           .data$blocked_opponent_shots,
           .data$takeaways,
           .data$giveaways), as.numeric) %>%
    dplyr::rename("team" = "name")

  s <- shot %>%
    dplyr::left_join(score, by = c("team")) %>%
    dplyr::mutate(
      team = tolower(.data$team),
      shootout_made_scoring = as.numeric(.data$shootout_made_scoring))

  df <- df %>%
    dplyr::left_join(s, by = c("team"))

  df <- df %>%
    dplyr::mutate(
      winner = ifelse(.data$total_scoring == max(.data$total_scoring, na.rm = TRUE), TRUE, FALSE))

  return(df)

}



#' @title process_phf_period
#' @description process_phf_period: processes the raw data of a single period from a PHF game
#'
#' @param data the dataframe of the period that you want parsed into a workable format of pbp data
#' @param period which period of play is this data for? Defaults to 1
#' @importFrom dplyr mutate row_number rename filter
#' @importFrom janitor clean_names remove_empty
#' @importFrom stringr str_detect
#' @export
process_phf_period <- function(data, period = 1) {

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

#' @title process_phf_shootout
#' @description process_phf_shootout: processes the raw data of a shootout from a PHF game
#'
#' @param data the dataframe of the shootout that you want parsed into a workable format of pbp data
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate row_number select
#' @importFrom stringr str_extract str_replace_all str_replace str_detect
#' @importFrom tidyr separate
#' @export
process_phf_shootout <- function(data) {
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
      desc2 = stringr::str_replace(.data$desc2, .data$first_number, ""),
      second_number = stringr::str_extract(.data$desc2, "#[0-9]+"),
      desc2 = stringr::str_replace(.data$desc2, .data$second_number, ","),
      first_number = stringr::str_trim(stringr::str_replace(.data$first_number, "#", "")),
      second_number = stringr::str_trim(stringr::str_replace(.data$second_number, "#", "")))

  # running separate on the comma separated names to extract player names
  # wrapped in `suppressWarnings()` to prevent it from throwing an error in weird cases about NAs being put in
  suppressWarnings(
    data <- data %>%
      tidyr::separate(.data$desc2, into = c("first_player", "second_player"),
                      sep = ","))

  data <- data %>%
    # trimming off whitespace from player names
    dplyr::mutate(
      first_player = stringr::str_trim(.data$first_player),
      second_player = stringr::str_trim(.data$second_player)) %>%
    dplyr::select(-c(.data$desc)) %>%
    # adding an extra line of cleaning in bc things sometimes remained weird
    dplyr::mutate(
      first_player = stringr::str_trim(stringr::str_replace(.data$first_player,
                                                            "missed attempt|scores", "")),
      second_player = stringr::str_trim(stringr::str_replace(.data$second_player,
                                                             "Shootout|Shoout|shoout|shootout", ""))
    )

  return(data)

}
