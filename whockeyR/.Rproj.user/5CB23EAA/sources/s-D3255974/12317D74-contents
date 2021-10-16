## the functions here still need work to account for the variation
## in how the NWHL/PHF does weird data structure stuff

shot_data <- function(data) {

  score <- data[[9]]
  shot <- data[[10]]

  score <- score %>%
    clean_names() %>%
    rename("team" = "scoring",
           "first_scoring" = "x1st",
           "second_scoring" = "x2nd",
           "third_scoring" = "x3rd",
           "total_scoring" = "t")

  shot <- shot %>%
    clean_names() %>%
    rename("team" = "shots",
           "first_shots" = "x1st",
           "second_shots" = "x2nd",
           "third_shots" = "x3rd",
           "total_shots" = "t")

  shot %>%
    left_join(score, by = c("team"))

}

process_boxscore <- function(data) {

  df <- data[[11]]
  score <- data[[9]]
  shot <- data[[10]]

  score <- score %>%
    clean_names() %>%
    rename("team" = "scoring",
           "first_scoring" = "x1st",
           "second_scoring" = "x2nd",
           "third_scoring" = "x3rd",
           "total_scoring" = "t")

  shot <- shot %>%
    clean_names() %>%
    rename("team" = "shots",
           "first_shots" = "x1st",
           "second_shots" = "x2nd",
           "third_shots" = "x3rd",
           "total_shots" = "t")

  df <- df %>%
    clean_names() %>%
    pivot_longer(cols = 2:3) %>%
    pivot_wider(names_from = team_stats) %>%
    clean_names() %>%
    separate(power_plays,
             into = c("successful_power_play", "power_play_opportunities"),
             sep = " / ") %>%
    mutate_at(vars(successful_power_play,
                   power_play_opportunities,
                   power_play_percent,
                   penalty_minutes,
                   faceoff_percent,
                   blocked_opponent_shots,
                   takeaways,
                   giveaways), as.numeric) %>%
    rename("team" = "name")

  s <- shot %>%
    left_join(score, by = c("team")) %>%
    mutate(team = tolower(team))

  df <- df %>%
    left_join(s, by = c("team"))

  return(df)

}

load_boxscore <- function(game_id = 268078) {

  df <- load_raw_data(game_id = game_id)

  df <- process_boxscore(data = df)

  return(df)

}
