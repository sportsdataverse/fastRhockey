source('R/pbp_functions.R')

# pkgload::load_all()

a <- Sys.time()

rg <- load_pbp(game_id = 268078)

Sys.time() - a

ot <- load_pbp(game_id = 268116)
so <- load_pbp(game_id = 268123)

Sys.time() - a

# g <- wehoop::espn_wnba_game_all(game_id = 401322870)
#
# g$Team

# raw <- load_raw_data(game_id = 268078)
#
# h <- c("away", "home")
#
# ah <- data.frame(h)
#
# rdw <- load_raw_data(game_id = 268116)
#
# bdc <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_nwhl.csv")
#
# tm <- raw[[max(length(raw)) - 1]] %>%
#   clean_names() %>%
#   mutate(
#     order = row_number(),
#     meta = ifelse(
#       order == 1, "away_team", ifelse(
#         order == 2, "home_team", NA
#       )
#     )
#   ) %>%
#   dplyr::select(shots, meta) %>%
#   pivot_wider(values_from = shots,
#               names_from = meta)
#
# rg <- rg %>%
#   left_join(tm, by = character())
#
# gl <- rg %>%
#   filter(event == "Goalie") %>%
#   # filter(str_detect(description, "Starting|Returned"))
#   dplyr::select(home_team, away_team, team, description,
#                 first_player, event, sec_from_start) %>%
#   mutate(
#     goalie_change = str_extract(description, "Starting|Returned|Pulled"),
#     goalie = ifelse(
#       str_detect(team, away_team), "away_goalie",
#       ifelse(
#         str_detect(team, home_team), "home_goalie", NA
#       )
#     ),
#     first_player = ifelse(goalie_change == "Pulled", "None", first_player)
#   ) %>%
#   dplyr::select(first_player, sec_from_start, goalie_change, goalie) %>%
#   pivot_wider(names_from = goalie,
#               values_from = first_player)
#
# rg <- rg %>%
#   left_join(gl, by = c("sec_from_start"))
#
# rg <- rg %>%
#   fill(home_goalie) %>%
#   fill(away_goalie) %>%
#   dplyr::filter(event != "Goalie") %>%
#   mutate(
#     home_goalie = ifelse(home_goalie == "None", NA, home_goalie),
#     away_goalie = ifelse(away_goalie == "None", NA, away_goalie),
#     goalie_involved = ifelse(event %in% c("Goal", "PP Goal", "Shot", "Shot BLK") &
#       str_detect(team, home_team), away_goalie,
#       ifelse(event %in% c("Goal", "PP Goal", "Shot", "Shot BLK") &
#          str_detect(team, away_team), home_goalie, NA
#       )
#     ),
#     time_elapsed = time,
#     time_remaining = clock
#   )
#
# r <- rg %>%
#   dplyr::select(game_id, home_team, away_team, period_id, event_no, description, time_remaining, on_ice_situation,
#                 home_goals, away_goals, leader, team, event,
#                 first_player, first_number, second_player, second_number, third_player, third_number,
#                 shot_type, shot_result, goalie_involved,
#                 penalty, penalty_length, penalty_type, penalty_called,
#                 offensive_player_one, offensive_player_two, offensive_player_three,
#                 offensive_player_four, offensive_player_five,
#                 home_goalie, away_goalie)

