# library(tidyverse)
# library(stringr)
# library(janitor)
#
# source('R/pbp_functions.R')
# source('R/phf_schedule.R')
# source('R/utils.R')
#
# x <- 379254
# y <- 268123
#
# data <- load_pbp(game_id = y)
#
# # so <- data %>%
# #   filter(on_ice_situation == "shootout") %>%
# #   mutate(
# #     first_player = stringr::str_trim(stringr::str_replace(first_player,
# #                                                           "missed attempt|scores", "")),
# #     second_player = stringr::str_trim(stringr::str_replace(second_player,
# #                                                            "Shootout|Shoout|shoout|shootout", ""))
# #   )
#
# # data <- load_raw_data(game_id = y)
#
# # score <- score %>%
# #   janitor::clean_names() %>%
# #   dplyr::rename("team" = "scoring",
# #                 "first_scoring" = "x1st",
# #                 "second_scoring" = "x2nd",
# #                 "third_scoring" = "x3rd",
# #                 "overtime_scoring" = "ot",
# #                 "shootout_scoring" = "so",
# #                 "total_scoring" = "t")
# #
# # score %>%
# #   dplyr::mutate(
# #     shootout_scoring = str_replace(shootout_scoring, "[0-9] ", ""),
# #     shootout_scoring = str_replace(shootout_scoring, "\\(", ""),
# #     shootout_scoring = str_replace(shootout_scoring, "\\)", ""),
# #     shootout_rep = str_replace(shootout_scoring, " - ", ",")) %>%
# #   dplyr::select(-c(shootout_scoring)) %>%
# #   tidyr::separate(shootout_rep, into = c("shootout_scoring", "shootout_shots"),
# #            sep = ",", remove = TRUE)
# #
# # score_string <- "[:digit:] - [:digit:] [A-Z]+|[:digit:] - [:digit:]"
# # shoot <- "missed attempt against|scores against|Shootout"
# # all <- "[:digit:] - [:digit:] [A-Z]+|[:digit:] - [:digit:]|missed attempt against|scores against|Shootout"
# #
# # as <- shootout %>%
# #   janitor::clean_names() %>%
# #   # creating variables, cleaning stuff for shootouts specifically
# #   # since there's a lot less variation in what can happen
# #   # it's easier to do the cleaning so it's in its own function
# #   dplyr::mutate(
# #     event = "Shootout",
# #     on_ice_situation = "shootout",
# #     shot_type = "shootout",
# #     shot_result = tolower(.data$x),
# #     period_id = 5,
# #     event_no = dplyr::row_number(),
# #     description = .data$play,
# #     desc = stringr::str_replace_all(.data$play, "#", ""),
# #     desc2 = stringr::str_replace_all(.data$description, all, ""),
# #     first_number = stringr::str_extract(.data$desc2, "#[0-9]+"),
# #     desc2 = stringr::str_replace(.data$desc2, first_number, ""),
# #     second_number = stringr::str_extract(.data$desc2, "#[0-9]+"),
# #     desc2 = stringr::str_replace(.data$desc2, second_number, ","),
# #     first_number = stringr::str_trim(stringr::str_replace(.data$first_number, "#", "")),
# #     second_number = stringr::str_trim(stringr::str_replace(.data$second_number, "#", ""))) %>%
# #   tidyr::separate(desc2, into = c("first_player", "second_player"),
# #            sep = ",") %>%
# #   dplyr::mutate(first_player = stringr::str_trim(first_player),
# #                 second_player = stringr::str_trim(second_player))
# #
# # data <- data %>%
# #   janitor::clean_names() %>%
# #   # creating variables, cleaning stuff for shootouts specifically
# #   # since there's a lot less variation in what can happen
# #   # it's easier to do the cleaning so it's in its own function
# #   dplyr::mutate(
# #     event = "Shootout",
# #     on_ice_situation = "shootout",
# #     shot_type = "shootout",
# #     shot_result = tolower(.data$x),
# #     period_id = 5,
# #     event_no = dplyr::row_number(),
# #     description = .data$play,
# #     desc = stringr::str_replace_all(.data$play, "#", ""),
# #     # first_number = str_nth_number(.data$desc, 1),
# #     # second_number = str_nth_number(.data$desc, 2),
# #     desc = str_replace_all(.data$desc, shoot, ""),
# #     score = str_extract(.data$desc, score_string),
# #     desc = str_replace_all(.data$desc, score_string, ""),
# #     desc = str_replace_all(str_trim(.data$desc, side = "both"),"#", ""),
# #     # first_player = str_nth_non_numeric(.data$desc, n = 1),
# #     # second_player = str_nth_non_numeric(.data$desc, n = 2),
# #     leader = str_extract(.data$score, "[A-Z]+"),
# #     scr = str_replace_all(.data$score, "[A-Z]+", "")) %>%
# #   dplyr::select(-.data$play) %>%
# #   tidyr::separate(
# #     .data$scr,
# #     into = c("away_goals", "home_goals"),
# #     sep = " - ", remove = FALSE) %>%
# #   dplyr::select(-.data$scr, -.data$x) %>%
# #   dplyr::mutate(
# #     leader = ifelse(is.na(.data$leader), 'T', .data$leader),
# #     away_goals = ifelse(is.na(.data$away_goals), 0, .data$away_goals),
# #     home_goals = ifelse(is.na(.data$home_goals), 0, .data$home_goals),
# #     score = ifelse(is.na(.data$score), '0 - 0 T', .data$score))

