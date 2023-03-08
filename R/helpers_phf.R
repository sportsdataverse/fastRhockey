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
#' @noRd
helper_phf_pbp_normalize_columns <- function(df){
  if (ncol(df) == 3) {
    colnames(df) <- c("play_type","team","play_description")
    df$time <- NA_character_
    df <- df %>%
      dplyr::select("play_type", "team", "time", "play_description")
  }
  if (ncol(df) == 10) {

    colnames(df) <- c("play_type", "team", "time","play_description","drop1","drop2",
                      "scoring_team_abbrev","scoring_team_on_ice","defending_team_abbrev","defending_team_on_ice")
    df2 <- df[,7:10]
    df2 <- df2 %>%
      dplyr::mutate_at(1:4, function(x){dplyr::lead(x,n=2)})
    df <- dplyr::bind_cols(df[,1:4], df2)
    df <- df %>%
      dplyr::filter(!is.na(.data$play_description),!stringr::str_detect(.data$team,"On Ice")) %>%
      dplyr::mutate(play_description = gsub("{{.*", "", .data$play_description, perl = TRUE))
  } else {
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
    dplyr::select("play_type", "team", "time", "play_description",
                  "scoring_team_abbrev","scoring_team_on_ice",
                  "defending_team_abbrev", "defending_team_on_ice")
  return(df)
}


#' @title phf_pbp_data
#' @description phf_pbp_data: returns all of the play-by-play data for a game into on big data frame using the process_phf_period/shootout functions. Contains functionality to account for regulation games, overtime games, and shootouts
#'
#' @param data the raw list data that is generated from the phf_game_raw function
#' @importFrom dplyr mutate bind_rows filter row_number select case_when pull starts_with ends_with
#' @importFrom tidyr pivot_wider separate fill replace_na
#' @importFrom stringr str_replace str_replace_all str_extract str_extract_all str_detect str_trim
#' @importFrom utils read.csv
#' @import rvest
#' @noRd
helper_phf_pbp_data <- function(data) {
  away <- "[:digit:] GvA|[:digit:] TkA|[:digit:] Blk"
  fill <- "from| by|against|to| and|giveaway|Game|Behind|of |Served|served|Bench|bench"
  goalie <- "Starting goalie|Pulled goalie|Returned goalie"
  fo <- "faceoff won"
  ice <- "Even Strength|Empty Net|Power Play|Extra Attacker|Short Handed"
  shots <- "Snap shot|Wrist shot|Penalty Shot"
  reb <- "blocked|saved|failed attempt"
  pen <- "Holding the Stick|Holding|Tripping|Roughing|Hooking|Interference|Diving|Delay|Cross-Checking|Head Contact|Body Checking|Slashing|Check from Behind Misconduct|Checking from Behind|Checking|Ejection|Too Many Men|Delay of Game|Misconduct|Check|High-Sticking|Game Misconduct"
  type <- "Minor|Major"
  score_string <- "[:digit:] - [:digit:] [A-Z]+|[:digit:] - [:digit:]"
  shoot <- "missed attempt against|scores against|Shootout|failed attempt"
  lgh <- "[:digit:] mins|[0-9]+ mins"
  abbreviations <- "TOR|MIN|BOS|CTW|MET|BUF|MON"
  ne <- "On Ice"


  pbp <- data %>%
    dplyr::mutate(
      team = stringr::str_replace(.data$team, abbreviations, ""),
      home_goalie = ifelse(.data$play_type == "Goalie" &
                           .data$team == .data$home_team,
                           stringr::str_remove(stringr::str_extract(.data$play_description,"\\d+ (.{0,25})"),"\\d+ "),
                           NA_character_),
      away_goalie = ifelse(.data$play_type == "Goalie" &
                             .data$team == .data$away_team,
                           stringr::str_remove(stringr::str_extract(.data$play_description,"\\d+ (.{0,25})"),"\\d+ "),
                           NA_character_),
      home_goalie_jersey = ifelse(.data$play_type == "Goalie" &
                                    .data$team == .data$home_team,
                                  stringr::str_extract(.data$play_description,"\\d+"),
                                  NA_character_),
      away_goalie_jersey = ifelse(.data$play_type == "Goalie" &
                                    .data$team == .data$away_team,
                                  stringr::str_extract(.data$play_description,"\\d+"),
                                  NA_character_),
      goalie_change =  ifelse(.data$play_type == "Goalie",
                              stringr::str_extract(.data$play_description, "Starting|Returned|Pulled"),
                              NA_character_),
      penalty = ifelse(.data$play_type == "Penalty", 1, 0),
      penalty_type = ifelse(.data$play_type == "Penalty",
                            stringr::str_extract(.data$play_description, pen),
                            NA_character_),
      penalty_level = ifelse(.data$play_type == "Penalty",
                              stringr::str_extract(.data$play_description,type),
                              NA_character_),
      penalty_length = ifelse(.data$play_type == "Penalty",
                             stringr::str_extract(.data$play_description,"[:digit:] mins"),
                             NA_character_),
      # replacing some basic stuff
      on_ice_situation = stringr::str_extract(.data$play_description, ice),
      # cleaning the on-ice situation
      shot_type = stringr::str_extract(.data$play_description, shots),
      shot_result = ifelse(stringr::str_detect(.data$play_type, "Goal") &
                           .data$play_type != "Goalie", "made",
                           stringr::str_extract(.data$play_description, reb)),
      score = stringr::str_extract(.data$play_description, score_string),) %>%
    tidyr::fill("home_goalie") %>%
    tidyr::fill("away_goalie") %>%
    tidyr::fill("home_goalie_jersey") %>%
    tidyr::fill("away_goalie_jersey")

  suppressWarnings(pbp <- pbp %>%
                     tidyr::separate("time", into = c("minute", "second"),
                                     sep = ":", remove = FALSE))
  suppressWarnings(
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
      dplyr::select(-"minute", -"second")
  )

  suppressWarnings(
    pbp <- pbp %>%
      # using the comma separators, separate the string into offensive_player one through six
      tidyr::separate("scoring_team_on_ice", into = c("offensive_player_name_1", "offensive_player_name_2",
                                                          "offensive_player_name_3", "offensive_player_name_4",
                                                          "offensive_player_name_5", "offensive_player_name_6"),
                      sep = " #", remove = FALSE))
  suppressWarnings(
    pbp <- pbp %>%
      # using the comma separators, separate the string into offensive_player one through six
      tidyr::separate("defending_team_on_ice", into = c("defensive_player_name_1", "defensive_player_name_2",
                                                          "defensive_player_name_3", "defensive_player_name_4",
                                                          "defensive_player_name_5", "defensive_player_name_6"),
                      sep = " #", remove = FALSE))
  pbp <- pbp %>%
    dplyr::mutate(
      offensive_player_jersey_1 = stringr::str_trim(stringr::str_extract(.data$offensive_player_name_1, "\\d+")),
      offensive_player_name_1 = stringr::str_trim(stringr::str_replace(.data$offensive_player_name_1, "#\\d+", "")),
      offensive_player_jersey_2 = stringr::str_trim(stringr::str_extract(.data$offensive_player_name_2, "\\d+")),
      offensive_player_name_2 = stringr::str_trim(stringr::str_replace(.data$offensive_player_name_2, .data$offensive_player_jersey_2, "")),
      offensive_player_jersey_3 = stringr::str_trim(stringr::str_extract(.data$offensive_player_name_3, "\\d+")),
      offensive_player_name_3 = stringr::str_trim(stringr::str_replace(.data$offensive_player_name_3, .data$offensive_player_jersey_3, "")),
      offensive_player_jersey_4 = stringr::str_trim(stringr::str_extract(.data$offensive_player_name_4, "\\d+")),
      offensive_player_name_4 = stringr::str_trim(stringr::str_replace(.data$offensive_player_name_4, .data$offensive_player_jersey_4, "")),
      offensive_player_jersey_5 = stringr::str_trim(stringr::str_extract(.data$offensive_player_name_5, "\\d+")),
      offensive_player_name_5 = stringr::str_trim(stringr::str_replace(.data$offensive_player_name_5, .data$offensive_player_jersey_5, "")),
      # there are instances where a team pulls its goalie and has 6 skaters so this is designed to search for that case
      offensive_player_jersey_6 = stringr::str_trim(stringr::str_extract(.data$offensive_player_name_6, "\\d+")),
      # in the instance where there is NOT a 6th skater, doing a raw str_replace creates a NA and removes the player names
      # so this ifelse statement looks to see if there was a 6th player jersey and is so, then replace that jersey with a comma
      # otherwise it just pastes the description there without touching it
      offensive_player_name_6 = ifelse(! is.na(.data$offensive_player_jersey_6),
                                    stringr::str_trim(stringr::str_replace(.data$offensive_player_name_6, .data$offensive_player_jersey_6, "")), .data$offensive_player_name_6))
  pbp <- pbp %>%
    dplyr::mutate(
      defensive_player_jersey_1 = stringr::str_trim(stringr::str_extract(.data$defensive_player_name_1, "\\d+")),
      defensive_player_name_1 = stringr::str_trim(stringr::str_replace(.data$defensive_player_name_1, "#\\d+", "")),
      defensive_player_jersey_2 = stringr::str_trim(stringr::str_extract(.data$defensive_player_name_2, "\\d+")),
      defensive_player_name_2 = stringr::str_trim(stringr::str_replace(.data$defensive_player_name_2, .data$defensive_player_jersey_2, "")),
      defensive_player_jersey_3 = stringr::str_trim(stringr::str_extract(.data$defensive_player_name_3, "\\d+")),
      defensive_player_name_3 = stringr::str_trim(stringr::str_replace(.data$defensive_player_name_3, .data$defensive_player_jersey_3, "")),
      defensive_player_jersey_4 = stringr::str_trim(stringr::str_extract(.data$defensive_player_name_4, "\\d+")),
      defensive_player_name_4 = stringr::str_trim(stringr::str_replace(.data$defensive_player_name_4, .data$defensive_player_jersey_4, "")),
      defensive_player_jersey_5 = stringr::str_trim(stringr::str_extract(.data$defensive_player_name_5, "\\d+")),
      defensive_player_name_5 = stringr::str_trim(stringr::str_replace(.data$defensive_player_name_5, .data$defensive_player_jersey_5, "")),
      # there are instances where a team pulls its goalie and has 6 skaters so this is designed to search for that case
      defensive_player_jersey_6 = stringr::str_trim(stringr::str_extract(.data$defensive_player_name_6, "\\d+")),
      # in the instance where there is NOT a 6th skater, doing a raw str_replace creates a NA and removes the player names
      # so this ifelse statement looks to see if there was a 6th player jersey and is so, then replace that jersey with a comma
      # otherwise it just pastes the description there without touching it
      defensive_player_name_6 = ifelse(! is.na(.data$defensive_player_jersey_6),
                                    stringr::str_trim(stringr::str_replace(.data$defensive_player_name_6, .data$defensive_player_jersey_6, "")), .data$defensive_player_name_6))
  pbp <- pbp %>%
    dplyr::mutate(
      leader = stringr::str_extract(.data$score, "[A-Z]+"),
      scr = stringr::str_replace_all(.data$score, "[A-Z]+", "")) %>%
    tidyr::separate(
      "scr",
      into = c("away_goals", "home_goals"),
      sep = " - ",
      remove = FALSE) %>%
    dplyr::select(-"scr") %>%
    tidyr::fill("score") %>%
    tidyr::fill("leader") %>%
    tidyr::fill("away_goals") %>%
    tidyr::fill("home_goals") %>%
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
                                  as.numeric(str_extract(.data$penalty_length, '\\d')) * 60,
                                  NA_real_),
      start_power_play = ifelse(.data$penalty == 1, .data$sec_from_start, NA_real_),
      end_power_play = ifelse(.data$penalty == 1, .data$start_power_play + .data$power_play_seconds, NA_real_)) %>%
    tidyr::fill("start_power_play") %>%
    tidyr::fill("end_power_play") %>%
    dplyr::mutate(
      on_ice_situation = ifelse((.data$sec_from_start >= .data$start_power_play &
                                   .data$sec_from_start <= .data$end_power_play) |
                                  (.data$on_ice_situation == "Power Play"), "Power Play",
                                .data$on_ice_situation),
      on_ice_situation = tidyr::replace_na(.data$on_ice_situation, "Even Strength"),
      goalie_involved = dplyr::case_when(
        .data$play_type %in% c("Goal", "PP Goal", "Shot", "Shot BLK") &
          str_detect(.data$team, .data$home_team) ~ .data$away_goalie,
        .data$play_type %in% c("Goal", "PP Goal", "Shot", "Shot BLK") &
          str_detect(.data$team, .data$away_team) ~ .data$home_goalie,
        TRUE ~ NA_character_),
      time_elapsed = .data$time,
      time_remaining = .data$clock,
      power_play_seconds = ifelse(is.na(.data$power_play_seconds), 0,
                                  .data$power_play_seconds))
  pbp <- pbp %>%
    # taking the players and numbers involved in a play
    dplyr::mutate(
      desc2 = stringr::str_replace_all(.data$play_description, away, ""),
      desc2 = stringr::str_replace_all(.data$desc2, fill, ""),
      desc2 = stringr::str_replace_all(.data$desc2, goalie, ""),
      desc2 = stringr::str_replace_all(.data$desc2, fo, ""),
      desc2 = stringr::str_replace_all(.data$desc2, ice, ""),
      desc2 = stringr::str_replace_all(.data$desc2, shots, ""),
      desc2 = stringr::str_replace_all(.data$desc2, reb, ""),
      desc2 = stringr::str_replace_all(.data$desc2, pen, ""),
      desc2 = stringr::str_replace_all(.data$desc2, type, ""),
      desc2 = stringr::str_replace_all(.data$desc2, shoot, ""),
      desc2 = stringr::str_replace_all(.data$desc2, score_string, ""),
      desc2 = stringr::str_replace_all(.data$desc2, lgh, ""),
      player_jersey_1 = stringr::str_extract(.data$desc2, "#[0-9]+"),
      desc2 = stringr::str_replace(.data$desc2, .data$player_jersey_1, ""),
      # don't replace first number with a comma because there is no name in front of the first number
      player_jersey_2 = stringr::str_extract(.data$desc2, "#[0-9]+"),
      # since there isn't always a second or third player involved in a play, using an ifelse statement
      # to figure out if there was a player, then replacing them if so
      desc2 = ifelse(!is.na(.data$player_jersey_2),
                     stringr::str_replace(.data$desc2, .data$player_jersey_2, ","), .data$desc2),
      player_jersey_3 = stringr::str_trim(stringr::str_extract(.data$desc2, "#[0-9]+")),
      desc2 = ifelse(!is.na(.data$player_jersey_3),
                     stringr::str_replace(.data$desc2, .data$player_jersey_3, ","), .data$desc2),
      player_jersey_1 = stringr::str_trim(stringr::str_replace_all(.data$player_jersey_1, "#", "")),
      player_jersey_2 = stringr::str_trim(stringr::str_replace_all(.data$player_jersey_2, "#", "")),
      player_jersey_3 = stringr::str_trim(stringr::str_replace_all(.data$player_jersey_3, "#", "")))

  # running the player name separation within suppressWarnings to avoid getting 'NA, expected 3 arguments'
  # for plays with just one or two players involved
  suppressWarnings(
    pbp <- pbp %>%
      tidyr::separate(col = "desc2", into = c("player_name_1", "player_name_2", "player_name_3"),
                      sep = ",", remove = TRUE))

  # trim whitespace around player names
  pbp <- pbp %>%
    dplyr::mutate(
      player_name_1 = stringr::str_trim(.data$player_name_1),
      player_name_2 = stringr::str_trim(.data$player_name_2),
      player_name_3 = stringr::str_trim(.data$player_name_3))


  # figuring out how many skaters are on the ice at a single time
  away_state_changes <- pbp %>%
    dplyr::filter((.data$play_type == "PP Goal" & stringr::str_detect(.data$team, .data$home_team)) |
                    (.data$play_type == "Penalty" & stringr::str_detect(.data$team, .data$away_team))) %>%
    dplyr::select("play_type", "sec_from_start", "power_play_seconds") %>%
    dplyr::mutate(play_type = ifelse(.data$play_type == "Penalty", 1, 2),
                  prev.event = lag(.data$play_type),
                  prev.time = lag(.data$sec_from_start),
                  prev.length = lag(.data$power_play_seconds))

  away_pen_mat <- apply(away_state_changes,
                        1,
                        function(x) {
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
    dplyr::filter((.data$play_type == "PP Goal" & stringr::str_detect(.data$team, .data$away_team)) |
                    (.data$play_type == "Penalty" & stringr::str_detect(.data$team, .data$home_team))) %>%
    dplyr::select("play_type", "sec_from_start", "power_play_seconds") %>%
    dplyr::mutate(
      play_type = ifelse(.data$play_type == "Penalty",1,2),
      prev.event = lag(.data$play_type),
      prev.time = lag(.data$sec_from_start),
      prev.length = lag(.data$power_play_seconds))

  home_pen_mat <- apply(home_state_changes,
                        1,
                        function(x) {
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
  if (! is.null(hdim)) {

    home_skaters <- 5 + apply(home_pen_mat, 1, sum)

    home_skaters <- as.data.frame(home_skaters) %>%
      tibble::rownames_to_column("sec_from_start")%>%
      dplyr::mutate(sec_from_start = as.numeric(.data$sec_from_start))

  } else if (is.null(hdim)) {

    hsec <- seq(1, max(pbp$period_id) * 1200)
    h_skate <- 5

    home_skaters <- data.frame(hsec, h_skate)

    colnames(home_skaters) <- c("sec_from_start", "home_skaters")

  }

  suppressMessages(pbp <- left_join(pbp, home_skaters))
  suppressMessages(pbp <- left_join(pbp, away_skaters))
  pbp <- pbp %>%
    dplyr::mutate(
      player_name_1 = stringr::str_trim(stringr::str_replace(.data$player_name_1,
                                                            "missed attempt|scores", "")),
      player_name_2 = stringr::str_trim(stringr::str_replace(.data$player_name_2,
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
#' @noRd
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
      dplyr::select(dplyr::all_of(shootout_shot_names))

    score <- score %>%
      dplyr::mutate(
        shootout_made_scoring = stringr::str_replace(.data$shootout_made_scoring, "[0-9] ", ""),
        shootout_made_scoring = stringr::str_replace(.data$shootout_made_scoring, "\\(", ""),
        shootout_made_scoring = stringr::str_replace(.data$shootout_made_scoring, "\\)", ""),
        shootout_rep = stringr::str_replace(.data$shootout_made_scoring, " - ", ",")) %>%
      dplyr::select(-c("shootout_made_scoring")) %>%
      tidyr::separate("shootout_rep", into = c("shootout_made_scoring", "shootout_missed_scoring"),
                      sep = ",", remove = TRUE) %>%
      dplyr::mutate(shootout_missed_scoring = as.numeric(.data$shootout_missed_scoring))

  }

  df <- df %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(cols = 2:3) %>%
    tidyr::pivot_wider(names_from = "team_stats") %>%
    janitor::clean_names() %>%
    tidyr::separate(
      "power_plays",
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

#' @title phf_get_season_id
#' @description phf_get_season_id: returns the PHF season ID for a given year
#'
#' @param season the season
#' @importFrom dplyr case_when
#' @noRd
phf_get_season_id <- function(season) {
  season_id <- dplyr::case_when(
    season == 2023 ~ 4667,
    season == 2022 ~ 3372,
    season == 2021 ~ 2779,
    season == 2020 ~ 1950,
    season == 2019 ~ 2047,
    season == 2018 ~ 2046,
    season == 2017 ~ 2045,
    season == 2016 ~ 246,
    TRUE ~ NA_real_
  )

  return(season_id)
}
