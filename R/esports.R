# esports.R -- DEL / ICE Hockey League / EIHL / Metal Ligaen wrappers.
#
# Mirrors sdv-py sportsdataverse/hockey/esports_runtime.py, esports_parsers.py and
# the four generated del_esports / icehl_esports / eihl_esports / metal_esports
# families. The four leagues' scoring system writes one key layout into four public
# S3 buckets (keyless JSON, see esports_leagues.R); .esports_get() does the fetch,
# the season map (END year -> bucket START year) and the refusals; the per-route
# cores below (.esports_<route>) fetch and parse; the exported <league>_<route>
# functions are thin wrappers over them.
#
# Every id is an integer and every key space is PER BUCKET: DEL match_id 4444 and
# ICEHL match_id 4444 are different games, and none of them is a league website's
# game id. player_id is the `playerId` key space; `match_player_id` (lineups only)
# is the per-game roster entry and never joins to it.
#
# The parsers follow the sdv-py contract: snake_case columns, nested objects
# flattened to <parent>_<child>, list cells as JSON text, the returns-schema types
# on every frame (empty, partial or full), documented columns first and in order.
# Int64 columns that overflow R's 32-bit integer (epoch milliseconds) are numeric.

# Returns-schema types, column by column (sdv-py _esports_schemas.SCHEMAS).
.ES_SCHEMAS <- list(
  del_schedule = c(
    `match_id` = "integer", `start_date` = "character", `real_end_date` = "character",
    `league_id` = "integer", `league` = "character", `season` = "integer",
    `round` = "character", `home_id` = "integer", `home_name` = "character",
    `home_shortcut` = "character", `guest_id` = "integer", `guest_name` = "character",
    `guest_shortcut` = "character", `status` = "character", `overtime_count` = "integer",
    `extra_time` = "logical", `shooting` = "logical", `contumacy` = "logical",
    `final_score_home` = "integer", `final_score_guest` = "integer", `first_period_score_home` = "integer",
    `first_period_score_guest` = "integer", `second_period_score_home` = "integer", `second_period_score_guest` = "integer",
    `third_period_score_home` = "integer", `third_period_score_guest` = "integer", `actual_time_name` = "character",
    `actual_time_alias` = "character", `last_event_time` = "integer", `overtime_score_home` = "integer",
    `overtime_score_guest` = "integer", `shootout_score_home` = "integer", `shootout_score_guest` = "integer"
  ),
  del_team_player_stats = c(
    `player_id` = "integer", `holema_id` = "integer", `name` = "character",
    `firstname` = "character", `surname` = "character", `date_of_birth` = "character",
    `active` = "logical", `position` = "character", `stick` = "character",
    `weight` = "integer", `height` = "integer", `jersey` = "integer",
    `birth_country` = "character", `nationality` = "character", `nationality_short` = "character",
    `team_shortcut` = "character", `games` = "integer", `goals` = "integer",
    `goals_home` = "integer", `goals_away` = "integer", `assists` = "integer",
    `assists_home` = "integer", `assists_away` = "integer", `points` = "integer",
    `points_home` = "integer", `points_away` = "integer", `penalty_minutes` = "integer",
    `positive` = "integer", `negative` = "integer", `points_per_game` = "numeric",
    `shot_efficiency` = "numeric", `penalty_minutes_per_game` = "numeric", `pp_goals` = "integer",
    `sh_goals` = "integer", `gw_goals` = "integer", `shots_attempts` = "integer",
    `shots_on_goal` = "integer", `shots_on_goal_home` = "integer", `shots_on_goal_away` = "integer",
    `shots_missed` = "integer", `shots_blocked` = "integer", `blocked_shots_by_player` = "integer",
    `faceoffs_win` = "integer", `faceoffs_losses` = "integer", `faceoffs_count` = "integer",
    `time_on_ice` = "integer", `time_on_ice_pp` = "integer", `time_on_ice_sh` = "integer",
    `time_on_ice_per_game` = "numeric", `shifts` = "integer", `shifts_per_game` = "numeric"
  ),
  del_top_scorers = c(
    `player_id` = "integer", `name` = "character", `surname` = "character",
    `team_id` = "integer", `team_name` = "character", `team_shortcut` = "character",
    `goals` = "integer", `assists` = "integer", `points` = "integer",
    `shots` = "integer", `games` = "integer", `penalty_minutes` = "integer",
    `time_on_ice` = "integer"
  ),
  del_faceoffs = c(
    `id` = "integer", `time` = "integer", `realtime` = "character",
    `position` = "character", `position_shortcut` = "character", `winner_id` = "integer",
    `winner_name` = "character", `loser_id` = "integer", `loser_name` = "character"
  ),
  del_game_info = c(
    `actual_time_name` = "character", `actual_time_alias` = "character", `stadium` = "character",
    `number_of_viewers` = "integer", `referees_head_referee1_id` = "integer", `referees_head_referee1_name` = "character",
    `referees_head_referee2_id` = "integer", `referees_head_referee2_name` = "character", `referees_line_referee1_id` = "integer",
    `referees_line_referee1_name` = "character", `referees_line_referee2_id` = "integer", `referees_line_referee2_name` = "character",
    `team_info_home_id` = "integer", `team_info_home_name` = "character", `team_info_home_shortcut` = "character",
    `team_info_visitor_id` = "integer", `team_info_visitor_name` = "character", `team_info_visitor_shortcut` = "character",
    `trainers_visitor_head_coach_id` = "integer", `trainers_visitor_head_coach_name` = "character", `trainers_home_head_coach_id` = "integer",
    `trainers_home_head_coach_name` = "character", `team_bilance_home_win` = "integer", `team_bilance_visitor_win` = "integer",
    `results_extra_time` = "logical", `results_shooting` = "logical", `results_contumacy` = "logical",
    `results_score_final_score_home` = "integer", `results_score_final_score_guest` = "integer", `results_score_first_period_score_home` = "integer",
    `results_score_first_period_score_guest` = "integer", `results_score_second_period_score_home` = "integer", `results_score_second_period_score_guest` = "integer",
    `results_score_third_period_score_home` = "integer", `results_score_third_period_score_guest` = "integer", `best_players_home_id` = "integer",
    `best_players_home_name` = "character", `best_players_visitor_id` = "integer", `best_players_visitor_name` = "character",
    `overtime_count` = "integer", `last_event_time` = "integer"
  ),
  del_pbp = c(
    `event_index` = "integer", `period` = "character", `period_number` = "integer",
    `time` = "integer", `event_type` = "character", `event_id` = "integer",
    `team_side` = "character", `goalie_player_id` = "integer", `goalie_jersey` = "integer",
    `goalie_name` = "character", `current_score` = "character", `real_time` = "character",
    `scoreboard_time` = "integer", `balance` = "character", `extra_attacker` = "logical",
    `empty_net` = "logical", `shooting` = "logical", `technical` = "logical",
    `scorer_player_id` = "integer", `scorer_jersey` = "integer", `scorer_name` = "character",
    `assist1_player_id` = "integer", `assist1_jersey` = "integer", `assist1_name` = "character",
    `assist2_player_id` = "integer", `assist2_jersey` = "integer", `assist2_name` = "character",
    `plus_player_ids` = "character", `minus_player_ids` = "character", `create_time` = "integer",
    `penalized_player_id` = "integer", `penalized_jersey` = "integer", `penalized_name` = "character",
    `penalty_from_real_time` = "character", `penalty_from_scoreboard_time` = "integer", `penalty_to_real_time` = "character",
    `penalty_to_scoreboard_time` = "integer", `duration` = "integer", `disciplined_functionary` = "logical",
    `disciplined_bm` = "logical", `codename` = "character", `replaced_player_id` = "integer",
    `replaced_jersey` = "integer", `replaced_name` = "character", `outgoing_goalie_player_id` = "integer",
    `outgoing_goalie_jersey` = "integer", `outgoing_goalie_name` = "character", `team_id` = "integer",
    `team_shortcut` = "character", `goal_review` = "character", `on_ice_decision` = "character",
    `reason_for_review` = "character", `decision_after_review` = "character"
  ),
  del_roster = c(
    `team_side` = "character", `slot` = "character", `player_id` = "integer",
    `match_player_id` = "integer", `name` = "character", `surname` = "character",
    `jersey` = "integer", `position` = "character", `captain` = "logical",
    `starting_six` = "logical"
  ),
  del_shifts = c(
    `shift_id` = "integer", `start_time` = "integer", `start_real_time` = "character",
    `end_time` = "integer", `end_real_time` = "character", `player_id` = "integer",
    `player_name` = "character", `team_id` = "integer", `team_name` = "character",
    `duration` = "integer"
  ),
  del_player_box = c(
    `player_id` = "integer", `holema_id` = "integer", `name` = "character",
    `firstname` = "character", `surname` = "character", `date_of_birth` = "character",
    `active` = "logical", `position` = "character", `stick` = "character",
    `weight` = "integer", `height` = "integer", `jersey` = "integer",
    `birth_country` = "character", `nationality` = "character", `nationality_short` = "character",
    `team_shortcut` = "character", `games` = "integer", `goals` = "integer",
    `goals_home` = "integer", `goals_away` = "integer", `assists` = "integer",
    `assists_home` = "integer", `assists_away` = "integer", `points` = "integer",
    `points_home` = "integer", `points_away` = "integer", `penalty_minutes` = "integer",
    `positive` = "integer", `negative` = "integer", `points_per_game` = "numeric",
    `shot_efficiency` = "numeric", `penalty_minutes_per_game` = "numeric", `pp_goals` = "integer",
    `sh_goals` = "integer", `gw_goals` = "integer", `shots_attempts` = "integer",
    `shots_on_goal` = "integer", `shots_on_goal_home` = "integer", `shots_on_goal_away` = "integer",
    `shots_missed` = "integer", `shots_blocked` = "integer", `blocked_shots_by_player` = "integer",
    `faceoffs_win` = "integer", `faceoffs_losses` = "integer", `faceoffs_count` = "integer",
    `time_on_ice` = "integer", `time_on_ice_pp` = "integer", `time_on_ice_sh` = "integer",
    `time_on_ice_per_game` = "numeric", `shifts` = "integer", `shifts_per_game` = "numeric"
  ),
  del_goalie_box = c(
    `player_id` = "integer", `name` = "character", `surname` = "character",
    `team_id` = "integer", `team_name` = "character", `team_shortcut` = "character",
    `jersey` = "integer", `matches` = "integer", `won_matches` = "integer",
    `lost_matches` = "integer", `saves` = "integer", `goals_against` = "integer",
    `saves_percent` = "numeric", `average` = "numeric", `zeros` = "integer",
    `time_on_ice` = "integer", `seconds_per_game` = "numeric", `minutes_in_play` = "numeric"
  ),
  del_standings = c(
    `league_part_id` = "integer", `league_part_name` = "character", `state` = "character",
    `datetime` = "character", `team_id` = "integer", `part_team_id` = "integer",
    `name` = "character", `shortcut` = "character", `position` = "integer",
    `points` = "integer", `games` = "integer", `wins` = "integer",
    `wins_ot` = "integer", `ties` = "integer", `losses_ot` = "integer",
    `losses` = "integer", `score1` = "integer", `score2` = "integer",
    `only_so_wins` = "integer", `only_so_losts` = "integer", `only_ot_wins` = "integer",
    `only_ot_losts` = "integer", `bonus` = "integer", `ppg` = "numeric"
  ),
  del_shots = c(
    `match_id` = "integer", `date` = "character", `home_id` = "integer",
    `home_name` = "character", `home_shortcut` = "character", `visitor_id` = "integer",
    `visitor_name` = "character", `visitor_shortcut` = "character", `shot_id` = "integer",
    `player_id` = "integer", `jersey` = "integer", `first_name` = "character",
    `last_name` = "character", `team_id` = "integer", `match_shot_resutl_id` = "integer",
    `coordinate_x` = "numeric", `coordinate_y` = "numeric", `real_date` = "character",
    `polygon` = "character", `time` = "integer", `shot_result_id` = "integer",
    `is_goal` = "logical", `team_side` = "character", `xg` = "numeric",
    `xg_type` = "character"
  ),
  icehl_schedule = c(
    `match_id` = "integer", `start_date` = "character", `real_start_date` = "character",
    `real_end_date` = "character", `league_id` = "integer", `league` = "character",
    `league_part_id` = "integer", `league_part_name` = "character", `season` = "integer",
    `round` = "character", `highlights` = "character", `match_number` = "integer",
    `bets_home_win` = "numeric", `bets_draw` = "numeric", `bets_visitor_win` = "numeric",
    `group` = "character", `home_id` = "integer", `home_name` = "character",
    `home_shortcut` = "character", `guest_id` = "integer", `guest_name` = "character",
    `guest_shortcut` = "character", `status` = "character", `overtime_count` = "integer",
    `extra_time` = "logical", `shooting` = "logical", `contumacy` = "logical",
    `final_score_home` = "integer", `final_score_guest` = "integer", `first_period_score_home` = "integer",
    `first_period_score_guest` = "integer", `second_period_score_home` = "integer", `second_period_score_guest` = "integer",
    `third_period_score_home` = "integer", `third_period_score_guest` = "integer", `actual_time_name` = "character",
    `actual_time_alias` = "character", `last_event_time` = "integer", `arena` = "character",
    `advanced_statistics` = "logical", `tickets_url` = "character", `television_name` = "character",
    `television_url` = "character", `television_logo` = "character", `television2` = "character"
  ),
  icehl_league_part_player_stats = c(
    `player_id` = "integer", `name` = "character", `firstname` = "character",
    `surname` = "character", `birthdate` = "character", `position` = "character",
    `stick` = "character", `weight` = "integer", `height` = "integer",
    `jersey` = "integer", `birth_country` = "character", `nationality` = "character",
    `nationality_short` = "character", `team_id` = "integer", `team_shortcut` = "character",
    `games` = "integer", `goals` = "integer", `goals_home` = "integer",
    `goals_away` = "integer", `assists` = "integer", `assists_home` = "integer",
    `assists_away` = "integer", `points` = "integer", `points_home` = "integer",
    `points_away` = "integer", `penalty_minutes` = "integer", `positive` = "integer",
    `negative` = "integer", `points_per_game` = "numeric", `shot_efficiency` = "numeric",
    `penalty_minutes_per_game` = "numeric", `pp_goals` = "integer", `sh_goals` = "integer",
    `gw_goals` = "integer", `shots_attempts` = "integer", `shots_on_goal` = "integer",
    `shots_on_goal_home` = "integer", `shots_on_goal_away` = "integer", `shots_missed` = "integer",
    `shots_blocked` = "integer", `blocked_shots_by_player` = "integer", `faceoffs_win` = "integer",
    `faceoffs_losses` = "integer", `faceoffs_count` = "integer", `timeonice` = "numeric",
    `timeonice_eq` = "numeric", `timeonice_pp` = "numeric", `timeonice_sh` = "numeric",
    `hits` = "integer", `visible_games` = "logical", `visible_goals` = "logical",
    `visible_assists` = "logical", `visible_points` = "logical", `stars_games` = "integer",
    `stars_goals` = "integer", `stars_assists` = "integer", `stars_points` = "integer",
    `milestone_games` = "numeric", `milestone_goals` = "numeric", `milestone_assists` = "numeric",
    `milestone_points` = "numeric", `toi_gp` = "numeric", `last_goal` = "character",
    `rank` = "integer"
  ),
  icehl_team_player_stats = c(
    `player_id` = "integer", `name` = "character", `firstname` = "character",
    `surname` = "character", `photo` = "character", `birthdate` = "character",
    `active` = "logical", `position` = "character", `stick` = "character",
    `weight` = "integer", `height` = "integer", `jersey` = "integer",
    `birth_country` = "character", `nationality` = "character", `nationality_short` = "character",
    `on_roster` = "integer", `team_shortcut` = "character", `games` = "integer",
    `goals` = "integer", `goals_home` = "integer", `goals_away` = "integer",
    `assists` = "integer", `assists_home` = "integer", `assists_away` = "integer",
    `points` = "integer", `points_home` = "integer", `points_away` = "integer",
    `penalty_minutes` = "integer", `positive` = "integer", `negative` = "integer",
    `points_per_game` = "numeric", `shot_efficiency` = "numeric", `penalty_minutes_per_game` = "numeric",
    `pp_goals` = "integer", `sh_goals` = "integer", `gw_goals` = "integer",
    `shots_attempts` = "integer", `shots_on_goal` = "integer", `shots_on_goal_home` = "integer",
    `shots_on_goal_away` = "integer", `shots_missed` = "integer", `shots_blocked` = "integer",
    `blocked_shots_by_player` = "integer", `faceoffs_win` = "integer", `faceoffs_losses` = "integer",
    `faceoffs_count` = "integer", `timeonice` = "numeric", `timeonice_eq` = "numeric",
    `timeonice_pp` = "numeric", `timeonice_sh` = "numeric", `hits` = "integer",
    `xg` = "numeric", `rank` = "integer"
  ),
  icehl_top_scorers = c(
    `player_id` = "integer", `name` = "character", `surname` = "character",
    `photo` = "character", `birthdate` = "character", `team_id` = "integer",
    `team_name` = "character", `team_shortcut` = "character", `goals` = "integer",
    `assists` = "integer", `points` = "integer", `shots` = "integer",
    `penalty_minutes` = "integer"
  ),
  icehl_faceoffs = c(
    `id` = "integer", `time` = "integer", `realtime` = "character",
    `position` = "character", `position_shortcut` = "character", `winner_id` = "integer",
    `winner_name` = "character", `loser_id` = "integer", `loser_name` = "character"
  ),
  icehl_game_info = c(
    `actual_time_name` = "character", `actual_time_alias` = "character", `status` = "character",
    `stadium` = "character", `number_of_viewers` = "integer", `referees_head_referee1` = "character",
    `referees_head_referee2` = "character", `referees_line_referee1` = "character", `referees_line_referee2` = "character",
    `referees_head_referee1number` = "character", `referees_head_referee2number` = "character", `referees_line_referee1number` = "character",
    `referees_line_referee2number` = "character", `team_info_home_id` = "integer", `team_info_home_name` = "character",
    `team_info_home_shortcut` = "character", `team_info_visitor_id` = "integer", `team_info_visitor_name` = "character",
    `team_info_visitor_shortcut` = "character", `trainers_visitor_head_coach` = "character", `trainers_home_head_coach` = "character",
    `team_bilance_home_win` = "integer", `team_bilance_visitor_win` = "integer", `results_extra_time` = "logical",
    `results_shooting` = "logical", `results_contumacy` = "logical", `results_score_final_score_home` = "integer",
    `results_score_final_score_guest` = "integer", `results_score_first_period_score_home` = "integer", `results_score_first_period_score_guest` = "integer",
    `results_score_second_period_score_home` = "integer", `results_score_second_period_score_guest` = "integer", `results_score_third_period_score_home` = "integer",
    `results_score_third_period_score_guest` = "integer"
  ),
  icehl_pbp = c(
    `event_index` = "integer", `period` = "character", `period_number` = "integer",
    `time` = "integer", `event_type` = "character", `event_id` = "integer",
    `team_side` = "character", `goalie_player_id` = "integer", `goalie_jersey` = "integer",
    `goalie_name` = "character", `current_score` = "character", `real_time` = "character",
    `scoreboard_time` = "integer", `balance` = "character", `extra_attacker` = "logical",
    `empty_net` = "logical", `shooting` = "logical", `technical` = "logical",
    `scorer_player_id` = "integer", `scorer_jersey` = "integer", `scorer_name` = "character",
    `assist1_player_id` = "integer", `assist1_jersey` = "integer", `assist1_name` = "character",
    `assist2_player_id` = "integer", `assist2_jersey` = "integer", `assist2_name` = "character",
    `plus_player_ids` = "character", `minus_player_ids` = "character", `create_time` = "integer",
    `penalized_player_id` = "integer", `penalized_jersey` = "integer", `penalized_name` = "character",
    `penalty_from_real_time` = "character", `penalty_from_scoreboard_time` = "integer", `penalty_to_real_time` = "character",
    `penalty_to_scoreboard_time` = "integer", `duration` = "integer", `disciplined_functionary` = "logical",
    `disciplined_bm` = "logical", `codename` = "character", `outgoing_goalie_player_id` = "integer",
    `outgoing_goalie_jersey` = "integer", `outgoing_goalie_name` = "character", `replaced_player_id` = "integer",
    `replaced_jersey` = "integer", `replaced_name` = "character", `team_id` = "integer",
    `team_shortcut` = "character", `goal_review` = "character", `on_ice_decision` = "character",
    `reason_for_review` = "character", `decision_after_review` = "character"
  ),
  icehl_player_box = c(
    `player_id` = "integer", `name` = "character", `firstname` = "character",
    `surname` = "character", `photo` = "character", `birthdate` = "character",
    `active` = "logical", `position` = "character", `stick` = "character",
    `weight` = "integer", `height` = "integer", `jersey` = "integer",
    `birth_country` = "character", `nationality` = "character", `nationality_short` = "character",
    `on_roster` = "integer", `team_shortcut` = "character", `games` = "integer",
    `goals` = "integer", `goals_home` = "integer", `goals_away` = "integer",
    `assists` = "integer", `assists_home` = "integer", `assists_away` = "integer",
    `points` = "integer", `points_home` = "integer", `points_away` = "integer",
    `penalty_minutes` = "integer", `positive` = "integer", `negative` = "integer",
    `points_per_game` = "numeric", `shot_efficiency` = "numeric", `penalty_minutes_per_game` = "numeric",
    `pp_goals` = "integer", `sh_goals` = "integer", `gw_goals` = "integer",
    `shots_attempts` = "integer", `shots_on_goal` = "integer", `shots_on_goal_home` = "integer",
    `shots_on_goal_away` = "integer", `shots_missed` = "integer", `shots_blocked` = "integer",
    `blocked_shots_by_player` = "integer", `faceoffs_win` = "integer", `faceoffs_losses` = "integer",
    `faceoffs_count` = "integer", `timeonice` = "numeric", `timeonice_eq` = "numeric",
    `timeonice_pp` = "numeric", `timeonice_sh` = "numeric", `hits` = "integer",
    `xg` = "numeric", `top5` = "logical", `rank` = "integer"
  ),
  icehl_goalie_box = c(
    `player_id` = "integer", `name` = "character", `surname` = "character",
    `photo` = "character", `birthdate` = "character", `team_id` = "integer",
    `team_name` = "character", `team_shortcut` = "character", `team_type` = "character",
    `jersey` = "integer", `matches` = "integer", `won_matches` = "integer",
    `lost_matches` = "integer", `saves` = "integer", `goals_against` = "integer",
    `shots_against` = "integer", `saves_percent` = "numeric", `average` = "numeric",
    `zeros` = "integer", `time_on_ice` = "integer", `seconds_per_game` = "numeric",
    `minutes_in_play` = "numeric", `visible_games` = "logical", `visible_shutouts` = "logical",
    `visible_wins` = "logical", `stars_games` = "integer", `stars_shutouts` = "integer",
    `stars_wins` = "integer", `milestone_games` = "numeric", `milestone_shutouts` = "numeric",
    `milestone_wins` = "numeric", `xg` = "numeric", `xga` = "numeric",
    `gsax` = "numeric", `xs` = "numeric", `gate_zone_ur_goals_against` = "integer",
    `gate_zone_ur_shots_against` = "integer", `gate_zone_ur_saves` = "integer", `gate_zone_ur_saves_percent` = "numeric",
    `gate_zone_ul_goals_against` = "integer", `gate_zone_ul_shots_against` = "integer", `gate_zone_ul_saves` = "integer",
    `gate_zone_ul_saves_percent` = "numeric", `gate_zone_br_goals_against` = "integer", `gate_zone_br_shots_against` = "integer",
    `gate_zone_br_saves` = "integer", `gate_zone_br_saves_percent` = "numeric", `gate_zone_fh_goals_against` = "integer",
    `gate_zone_fh_shots_against` = "integer", `gate_zone_fh_saves` = "integer", `gate_zone_fh_saves_percent` = "numeric",
    `gate_zone_bl_goals_against` = "integer", `gate_zone_bl_shots_against` = "integer", `gate_zone_bl_saves` = "integer",
    `gate_zone_bl_saves_percent` = "numeric", `gate_zone_ce_goals_against` = "integer", `gate_zone_ce_shots_against` = "integer",
    `gate_zone_ce_saves` = "integer", `gate_zone_ce_saves_percent` = "numeric", `rank` = "integer",
    `team_time_on_ice` = "integer"
  ),
  icehl_standings = c(
    `group_id` = "integer", `group_name` = "character", `complete` = "logical",
    `team_id` = "integer", `part_team_id` = "integer", `name` = "character",
    `team_original` = "character", `shortcut` = "character", `position` = "integer",
    `points` = "integer", `games` = "integer", `wins` = "integer",
    `wins_ot` = "integer", `wins_reg` = "integer", `ties` = "integer",
    `losts_ot` = "integer", `losts` = "integer", `score1` = "integer",
    `score2` = "integer", `goals_away` = "integer", `points_away` = "integer",
    `last10` = "character", `only_ot_wins` = "integer", `only_so_wins` = "integer",
    `only_ot_losts` = "integer", `only_so_losts` = "integer", `qualified` = "logical",
    `live` = "logical", `live_game_score` = "character", `position_change` = "integer",
    `ppg` = "numeric"
  ),
  icehl_shots = c(
    `match_id` = "integer", `date` = "character", `home_id` = "integer",
    `home_name` = "character", `home_shortcut` = "character", `visitor_id` = "integer",
    `visitor_name` = "character", `visitor_shortcut` = "character", `status` = "character",
    `shot_id` = "integer", `player_id` = "integer", `jersey` = "integer",
    `first_name` = "character", `last_name` = "character", `team_id` = "integer",
    `match_shot_resutl_id` = "integer", `time` = "integer", `coordinate_x` = "numeric",
    `coordinate_y` = "numeric", `real_date` = "character", `xg` = "numeric",
    `xg_type` = "character", `shot_result_id` = "integer", `is_goal` = "logical",
    `team_side` = "character"
  ),
  icehl_game_center = c(
    `status_id` = "integer", `status_msg` = "character", `data_game_data_id` = "integer",
    `data_game_data_division_id` = "integer", `data_game_data_division_longname` = "character", `data_game_data_division_shortname` = "character",
    `data_game_data_scheduled_date_formatted_short` = "character", `data_game_data_scheduled_date_formatted_long` = "character", `data_game_data_scheduled_date_timestamp` = "numeric",
    `data_game_data_scheduled_date_value` = "character", `data_game_data_scheduled_date_diff_to_now` = "integer", `data_game_data_scheduled_time` = "character",
    `data_game_data_start_time` = "character", `data_game_data_end_time` = "character", `data_game_data_is_overtime` = "integer",
    `data_game_data_is_shoot_out` = "integer", `data_game_data_home_team_id` = "integer", `data_game_data_home_team_longname` = "character",
    `data_game_data_home_team_shortname` = "character", `data_game_data_home_team_flavourname` = "character", `data_game_data_home_team_score` = "integer",
    `data_game_data_away_team_id` = "integer", `data_game_data_away_team_longname` = "character", `data_game_data_away_team_shortname` = "character",
    `data_game_data_away_team_flavourname` = "character", `data_game_data_away_team_score` = "integer", `data_game_data_game_status` = "integer",
    `data_game_data_game_extended_status` = "integer", `data_game_data_game_extended_status_info_text` = "character", `data_game_data_game_name` = "character",
    `data_game_data_game_day` = "character", `data_game_data_game_round` = "integer", `data_game_data_location_longname` = "character",
    `data_game_data_location_shortname` = "character", `data_game_data_location_coordinates_lat` = "integer", `data_game_data_location_coordinates_lng` = "integer",
    `data_game_data_attendance` = "integer", `data_game_data_live_time` = "integer", `data_game_data_live_time_formatted` = "character",
    `data_game_data_live_time_period` = "integer", `data_game_data_live_time_string` = "character", `data_game_data_live_time_game_phase` = "character",
    `data_game_data_period_stats` = "character", `data_game_data_game_officials_ref1_lastname` = "character", `data_game_data_game_officials_ref1_firstname` = "character",
    `data_game_data_game_officials_ref2_lastname` = "character", `data_game_data_game_officials_ref2_firstname` = "character", `data_game_data_game_officials_linesman1_lastname` = "character",
    `data_game_data_game_officials_linesman1_firstname` = "character", `data_game_data_game_officials_linesman2_lastname` = "character", `data_game_data_game_officials_linesman2_firstname` = "character",
    `data_game_data_team_officials_headcoachhome_lastname` = "character", `data_game_data_team_officials_headcoachhome_firstname` = "character", `data_game_data_team_officials_headcoachaway_lastname` = "character",
    `data_game_data_team_officials_headcoachaway_firstname` = "character", `data_game_data_live_time_period_bar` = "character", `data_game_data_you_tube_link` = "character",
    `data_game_data_number_of_periods` = "integer", `data_game_data_length_of_period` = "integer", `data_game_data_number_of_overtimes` = "integer",
    `data_game_data_length_of_overtime` = "integer", `data_game_data_shootout_shots` = "integer", `data_game_data_images_home_team_logo` = "character",
    `data_game_data_images_away_team_logo` = "character", `data_game_data_streams` = "character", `data_game_data_home_team_logo_url` = "character",
    `data_game_data_away_team_logo_url` = "character", `data_home_goals` = "character", `data_home_penalties` = "character",
    `data_home_field_players` = "character", `data_home_goal_keepers` = "character", `data_home_team_stats_faceoffs_won` = "integer",
    `data_home_team_stats_faceoffs_total` = "integer", `data_home_team_stats_shots_on_goal` = "integer", `data_home_team_stats_shots_blocked` = "integer",
    `data_home_team_stats_penalty_minutes` = "integer", `data_home_team_stats_powerplay_goals` = "integer", `data_home_team_stats_powerplay_opportunities` = "integer",
    `data_home_team_stats_penaltykill_successes` = "integer", `data_home_team_stats_penaltykill_situations` = "integer", `data_home_team_stats_time_values_leading` = "integer",
    `data_home_team_stats_time_values_tied` = "integer", `data_home_team_stats_time_values_trailing` = "integer", `data_home_team_stats_time_values_time_game_strength_even` = "integer",
    `data_home_team_stats_time_values_time_game_strength_plus_one` = "integer", `data_home_team_stats_time_values_time_game_strength_plus_two` = "integer", `data_home_team_stats_time_values_time_game_strength_minus_one` = "integer",
    `data_home_team_stats_time_values_time_game_strength_minus_two` = "integer", `data_home_team_stats_time_values_time_on_powerplay` = "integer", `data_home_team_stats_time_values_time_while_shorthanded` = "integer",
    `data_away_goals` = "character", `data_away_penalties` = "character", `data_away_field_players` = "character",
    `data_away_goal_keepers` = "character", `data_away_team_stats_faceoffs_won` = "integer", `data_away_team_stats_faceoffs_total` = "integer",
    `data_away_team_stats_shots_on_goal` = "integer", `data_away_team_stats_shots_blocked` = "integer", `data_away_team_stats_penalty_minutes` = "integer",
    `data_away_team_stats_powerplay_goals` = "integer", `data_away_team_stats_powerplay_opportunities` = "integer", `data_away_team_stats_penaltykill_successes` = "integer",
    `data_away_team_stats_penaltykill_situations` = "integer", `data_away_team_stats_time_values_leading` = "integer", `data_away_team_stats_time_values_tied` = "integer",
    `data_away_team_stats_time_values_trailing` = "integer", `data_away_team_stats_time_values_time_game_strength_even` = "integer", `data_away_team_stats_time_values_time_game_strength_plus_one` = "integer",
    `data_away_team_stats_time_values_time_game_strength_plus_two` = "integer", `data_away_team_stats_time_values_time_game_strength_minus_one` = "integer", `data_away_team_stats_time_values_time_game_strength_minus_two` = "integer",
    `data_away_team_stats_time_values_time_on_powerplay` = "integer", `data_away_team_stats_time_values_time_while_shorthanded` = "integer", `data_home_goal_keeper_changes` = "character",
    `data_away_goal_keeper_changes` = "character", `data_game_actions` = "character", `data_faceoffs_head_to_head` = "character",
    `data_faceoffs_individual` = "character", `data_game_situations` = "character", `data_shootouts` = "character",
    `calc_time` = "integer", `last_update_formatted_short` = "character", `last_update_formatted_long` = "character",
    `last_update_timestamp` = "numeric", `last_update_value` = "character", `served_by_server` = "character"
  ),
  eihl_schedule = c(
    `match_id` = "integer", `start_date` = "character", `league_id` = "integer",
    `league` = "character", `season` = "integer", `round` = "character",
    `group` = "character", `home_id` = "integer", `home_name` = "character",
    `home_shortcut` = "character", `guest_id` = "integer", `guest_name` = "character",
    `guest_shortcut` = "character", `status` = "character", `overtime_count` = "integer",
    `extra_time` = "logical", `shooting` = "logical", `contumacy` = "logical",
    `final_score_home` = "integer", `final_score_guest` = "integer", `first_period_score_home` = "integer",
    `first_period_score_guest` = "integer", `second_period_score_home` = "integer", `second_period_score_guest` = "integer",
    `third_period_score_home` = "integer", `third_period_score_guest` = "integer", `actual_time_name` = "character",
    `actual_time_alias` = "character", `last_event_time` = "integer", `arena` = "character"
  ),
  eihl_team_player_stats = c(
    `player_id` = "integer", `name` = "character", `firstname` = "character",
    `surname` = "character", `birthdate` = "character", `active` = "logical",
    `position` = "character", `stick` = "character", `weight` = "integer",
    `height` = "integer", `jersey` = "integer", `birth_country` = "character",
    `nationality` = "character", `nationality_short` = "character", `team_shortcut` = "character",
    `games` = "integer", `goals` = "integer", `goals_home` = "integer",
    `goals_away` = "integer", `assists` = "integer", `assists_home` = "integer",
    `assists_away` = "integer", `points` = "integer", `points_home` = "integer",
    `points_away` = "integer", `penalty_minutes` = "integer", `positive` = "integer",
    `negative` = "integer", `points_per_game` = "numeric", `shot_efficiency` = "numeric",
    `penalty_minutes_per_game` = "numeric", `pp_goals` = "integer", `sh_goals` = "integer",
    `gw_goals` = "integer", `shots_attempts` = "integer", `shots_on_goal` = "integer",
    `shots_on_goal_home` = "integer", `shots_on_goal_away` = "integer", `shots_missed` = "integer",
    `shots_blocked` = "integer", `blocked_shots_by_player` = "integer", `faceoffs_win` = "integer",
    `faceoffs_losses` = "integer", `faceoffs_count` = "integer", `timeonice` = "numeric",
    `timeonice_eq` = "numeric", `timeonice_pp` = "numeric", `timeonice_sh` = "numeric",
    `hits` = "integer"
  ),
  eihl_top_scorers = c(
    `player_id` = "integer", `name` = "character", `surname` = "character",
    `jersey` = "integer", `birthdate` = "character", `team_id` = "integer",
    `team_name` = "character", `team_shortcut` = "character", `matches` = "integer",
    `goals` = "integer", `assists` = "integer", `points` = "integer",
    `shots` = "integer", `positives` = "integer", `negatives` = "integer",
    `positives_negatives` = "integer"
  ),
  eihl_game_info = c(
    `actual_time_name` = "character", `actual_time_alias` = "character", `stadium` = "character",
    `number_of_viewers` = "integer", `referees_head_referee1` = "character", `referees_head_referee2` = "character",
    `referees_line_referee1` = "character", `referees_line_referee2` = "character", `team_info_home_id` = "integer",
    `team_info_home_name` = "character", `team_info_home_shortcut` = "character", `team_info_visitor_id` = "integer",
    `team_info_visitor_name` = "character", `team_info_visitor_shortcut` = "character", `trainers_home_head_coach` = "character",
    `trainers_visitor_head_coach` = "character", `team_bilance_home_win` = "integer", `team_bilance_visitor_win` = "integer",
    `results_extra_time` = "logical", `results_shooting` = "logical", `results_contumacy` = "logical",
    `results_score_final_score_home` = "integer", `results_score_final_score_guest` = "integer", `results_score_first_period_score_home` = "integer",
    `results_score_first_period_score_guest` = "integer", `results_score_second_period_score_home` = "integer", `results_score_second_period_score_guest` = "integer",
    `results_score_third_period_score_home` = "integer", `results_score_third_period_score_guest` = "integer"
  ),
  eihl_pbp = c(
    `event_index` = "integer", `period` = "character", `period_number` = "integer",
    `time` = "integer", `event_type` = "character", `event_id` = "integer",
    `team_side` = "character", `goalie_player_id` = "integer", `goalie_jersey` = "integer",
    `goalie_name` = "character", `current_score` = "character", `real_time` = "character",
    `scoreboard_time` = "integer", `balance` = "character", `extra_attacker` = "logical",
    `empty_net` = "logical", `shooting` = "logical", `technical` = "logical",
    `scorer_player_id` = "integer", `scorer_jersey` = "integer", `scorer_name` = "character",
    `assist1_player_id` = "integer", `assist1_jersey` = "integer", `assist1_name` = "character",
    `assist2_player_id` = "integer", `assist2_jersey` = "integer", `assist2_name` = "character",
    `plus_player_ids` = "character", `minus_player_ids` = "character", `team_id` = "integer",
    `team_shortcut` = "character", `goal_review` = "character", `on_ice_decision` = "character",
    `reason_for_review` = "character", `decision_after_review` = "character", `outgoing_goalie_player_id` = "integer",
    `outgoing_goalie_jersey` = "integer", `outgoing_goalie_name` = "character", `create_time` = "integer",
    `penalized_player_id` = "integer", `penalized_jersey` = "integer", `penalized_name` = "character",
    `penalty_from_real_time` = "character", `penalty_from_scoreboard_time` = "integer", `penalty_to_real_time` = "character",
    `penalty_to_scoreboard_time` = "integer", `duration` = "integer", `disciplined_functionary` = "logical",
    `disciplined_bm` = "logical", `codename` = "character", `replaced_player_id` = "integer",
    `replaced_jersey` = "integer", `replaced_name` = "character"
  ),
  eihl_player_box = c(
    `player_id` = "integer", `name` = "character", `firstname` = "character",
    `surname` = "character", `birthdate` = "character", `active` = "logical",
    `position` = "character", `stick` = "character", `weight` = "integer",
    `height` = "integer", `jersey` = "integer", `birth_country` = "character",
    `nationality` = "character", `nationality_short` = "character", `team_shortcut` = "character",
    `games` = "integer", `goals` = "integer", `goals_home` = "integer",
    `goals_away` = "integer", `assists` = "integer", `assists_home` = "integer",
    `assists_away` = "integer", `points` = "integer", `points_home` = "integer",
    `points_away` = "integer", `penalty_minutes` = "integer", `positive` = "integer",
    `negative` = "integer", `points_per_game` = "numeric", `shot_efficiency` = "numeric",
    `penalty_minutes_per_game` = "numeric", `pp_goals` = "integer", `sh_goals` = "integer",
    `gw_goals` = "integer", `shots_attempts` = "integer", `shots_on_goal` = "integer",
    `shots_on_goal_home` = "integer", `shots_on_goal_away` = "integer", `shots_missed` = "integer",
    `shots_blocked` = "integer", `blocked_shots_by_player` = "integer", `faceoffs_win` = "integer",
    `faceoffs_losses` = "integer", `faceoffs_count` = "integer", `timeonice` = "numeric",
    `timeonice_eq` = "numeric", `timeonice_pp` = "numeric", `timeonice_sh` = "numeric",
    `hits` = "integer"
  ),
  eihl_goalie_box = c(
    `player_id` = "integer", `name` = "character", `surname` = "character",
    `birthdate` = "character", `team_id` = "integer", `team_name` = "character",
    `team_shortcut` = "character", `jersey` = "integer", `matches` = "integer",
    `won_matches` = "integer", `lost_matches` = "integer", `saves` = "integer",
    `goals_against` = "integer", `saves_percent` = "numeric", `average` = "numeric",
    `zeros` = "integer", `time_on_ice` = "integer", `seconds_per_game` = "numeric",
    `minutes_in_play` = "numeric", `visible_games` = "logical", `visible_shutouts` = "logical",
    `visible_wins` = "logical", `stars_games` = "integer", `stars_shutouts` = "integer",
    `stars_wins` = "integer", `milestone_games` = "numeric", `milestone_shutouts` = "numeric",
    `milestone_wins` = "numeric"
  ),
  eihl_standings = c(
    `group_id` = "integer", `group_name` = "character", `complete` = "logical",
    `team_id` = "integer", `part_team_id` = "integer", `name` = "character",
    `team_original` = "character", `shortcut` = "character", `position` = "integer",
    `points` = "integer", `games` = "integer", `wins` = "integer",
    `wins_ot` = "integer", `wins_reg` = "integer", `ties` = "integer",
    `losts_ot` = "integer", `losts` = "integer", `score1` = "integer",
    `score2` = "integer", `goals_away` = "integer", `last10` = "character",
    `only_ot_wins` = "integer", `only_so_wins` = "integer", `only_ot_losts` = "integer",
    `only_so_losts` = "integer", `qualified` = "logical", `ppg` = "numeric"
  ),
  eihl_shots = c(
    `match_id` = "integer", `date` = "character", `home_id` = "integer",
    `home_name` = "character", `home_shortcut` = "character", `visitor_id` = "integer",
    `visitor_name` = "character", `visitor_shortcut` = "character", `status` = "character",
    `shot_id` = "integer", `player_id` = "integer", `jersey` = "integer",
    `first_name` = "character", `last_name` = "character", `team_id` = "integer",
    `match_shot_resutl_id` = "integer", `time` = "integer", `coordinate_x` = "numeric",
    `coordinate_y` = "numeric", `real_date` = "character", `xg` = "numeric",
    `xg_type` = "character", `shot_result_id` = "integer", `is_goal` = "logical",
    `team_side` = "character"
  ),
  metal_schedule = c(
    `match_id` = "integer", `start_date` = "character", `real_start_date` = "character",
    `real_end_date` = "character", `league_id` = "integer", `league` = "character",
    `league_part_id` = "integer", `league_part_name` = "character", `season` = "integer",
    `round` = "character", `group` = "character", `tickets_url` = "character",
    `home_id` = "integer", `home_name` = "character", `home_shortcut` = "character",
    `guest_id` = "integer", `guest_name` = "character", `guest_shortcut` = "character",
    `status` = "character", `overtime_count` = "integer", `extra_time` = "logical",
    `shooting` = "logical", `contumacy` = "logical", `final_score_home` = "integer",
    `final_score_guest` = "integer", `first_period_score_home` = "integer", `first_period_score_guest` = "integer",
    `second_period_score_home` = "integer", `second_period_score_guest` = "integer", `third_period_score_home` = "integer",
    `third_period_score_guest` = "integer", `actual_time_name` = "character", `actual_time_alias` = "character",
    `last_event_time` = "integer", `arena` = "character", `advanced_statistics` = "logical"
  ),
  metal_top_scorers = c(
    `player_id` = "integer", `name` = "character", `surname` = "character",
    `birthdate` = "character", `team_id` = "integer", `team_name` = "character",
    `team_shortcut` = "character", `goals` = "integer", `assists` = "integer",
    `points` = "integer", `shots` = "integer"
  ),
  metal_game_info = c(
    `actual_time_name` = "character", `actual_time_alias` = "character", `status` = "character",
    `stadium` = "character", `number_of_viewers` = "integer", `referees_head_referee1` = "character",
    `referees_head_referee2` = "character", `referees_line_referee1` = "character", `referees_line_referee2` = "character",
    `referees_head_referee1number` = "integer", `referees_head_referee2number` = "integer", `referees_line_referee1number` = "integer",
    `referees_line_referee2number` = "integer", `referees_head_referee1id` = "integer", `referees_head_referee2id` = "integer",
    `referees_line_referee1id` = "integer", `referees_line_referee2id` = "integer", `team_info_home_id` = "integer",
    `team_info_home_name` = "character", `team_info_home_shortcut` = "character", `team_info_visitor_id` = "integer",
    `team_info_visitor_name` = "character", `team_info_visitor_shortcut` = "character", `trainers_home_head_coach` = "character",
    `trainers_visitor_head_coach` = "character", `team_bilance_home_win` = "integer", `team_bilance_visitor_win` = "integer",
    `results_extra_time` = "logical", `results_shooting` = "logical", `results_contumacy` = "logical",
    `results_score_final_score_home` = "integer", `results_score_final_score_guest` = "integer", `results_score_first_period_score_home` = "integer",
    `results_score_first_period_score_guest` = "integer", `results_score_second_period_score_home` = "integer", `results_score_second_period_score_guest` = "integer",
    `results_score_third_period_score_home` = "integer", `results_score_third_period_score_guest` = "integer"
  ),
  metal_pbp = c(
    `event_index` = "integer", `period` = "character", `period_number` = "integer",
    `time` = "integer", `event_type` = "character", `event_id` = "integer",
    `team_side` = "character", `goalie_player_id` = "integer", `goalie_jersey` = "integer",
    `goalie_name` = "character", `create_time` = "integer", `penalized_player_id` = "integer",
    `penalized_jersey` = "integer", `penalized_name` = "character", `penalty_from_real_time` = "character",
    `penalty_from_scoreboard_time` = "integer", `penalty_to_real_time` = "character", `penalty_to_scoreboard_time` = "integer",
    `duration` = "integer", `shooting` = "logical", `disciplined_functionary` = "logical",
    `disciplined_bm` = "logical", `codename` = "character", `current_score` = "character",
    `real_time` = "character", `scoreboard_time` = "integer", `balance` = "character",
    `extra_attacker` = "logical", `empty_net` = "logical", `technical` = "logical",
    `scorer_player_id` = "integer", `scorer_jersey` = "integer", `scorer_name` = "character",
    `assist1_player_id` = "integer", `assist1_jersey` = "integer", `assist1_name` = "character",
    `assist2_player_id` = "integer", `assist2_jersey` = "integer", `assist2_name` = "character",
    `plus_player_ids` = "character", `minus_player_ids` = "character", `replaced_player_id` = "integer",
    `replaced_jersey` = "integer", `replaced_name` = "character", `outgoing_goalie_player_id` = "integer",
    `outgoing_goalie_jersey` = "integer", `outgoing_goalie_name` = "character", `team_id` = "integer",
    `team_shortcut` = "character", `goal_review` = "character", `on_ice_decision` = "character",
    `reason_for_review` = "character", `decision_after_review` = "character"
  ),
  metal_player_box = c(
    `player_id` = "integer", `name` = "character", `firstname` = "character",
    `surname` = "character", `birthdate` = "character", `active` = "logical",
    `position` = "character", `stick` = "character", `weight` = "integer",
    `height` = "integer", `jersey` = "integer", `birth_country` = "character",
    `nationality` = "character", `nationality_short` = "character", `team_shortcut` = "character",
    `games` = "integer", `goals` = "integer", `goals_home` = "integer",
    `goals_away` = "integer", `assists` = "integer", `assists_home` = "integer",
    `assists_away` = "integer", `points` = "integer", `points_home` = "integer",
    `points_away` = "integer", `penalty_minutes` = "integer", `positive` = "integer",
    `negative` = "integer", `points_per_game` = "numeric", `shot_efficiency` = "numeric",
    `penalty_minutes_per_game` = "numeric", `pp_goals` = "integer", `sh_goals` = "integer",
    `gw_goals` = "integer", `shots_attempts` = "integer", `shots_on_goal` = "integer",
    `shots_on_goal_home` = "integer", `shots_on_goal_away` = "integer", `shots_missed` = "integer",
    `shots_blocked` = "integer", `blocked_shots_by_player` = "integer", `faceoffs_win` = "integer",
    `faceoffs_losses` = "integer", `faceoffs_count` = "integer", `timeonice` = "numeric",
    `timeonice_eq` = "numeric", `timeonice_pp` = "numeric", `timeonice_sh` = "numeric",
    `hits` = "integer", `xg` = "numeric", `top5` = "logical",
    `rank` = "integer"
  ),
  metal_goalie_box = c(
    `player_id` = "integer", `name` = "character", `surname` = "character",
    `photo` = "character", `birthdate` = "character", `team_id` = "integer",
    `team_name` = "character", `team_shortcut` = "character", `team_type` = "character",
    `jersey` = "integer", `matches` = "integer", `won_matches` = "integer",
    `lost_matches` = "integer", `saves` = "integer", `goals_against` = "integer",
    `shots_against` = "integer", `saves_percent` = "numeric", `average` = "numeric",
    `zeros` = "integer", `time_on_ice` = "integer", `seconds_per_game` = "numeric",
    `minutes_in_play` = "numeric", `visible_games` = "logical", `visible_shutouts` = "logical",
    `visible_wins` = "logical", `stars_games` = "integer", `stars_shutouts` = "integer",
    `stars_wins` = "integer", `milestone_games` = "numeric", `milestone_shutouts` = "numeric",
    `milestone_wins` = "numeric", `xg` = "numeric", `xga` = "numeric",
    `gsax` = "numeric", `xs` = "numeric", `gate_zone_ur_goals_against` = "integer",
    `gate_zone_ur_shots_against` = "integer", `gate_zone_ur_saves` = "integer", `gate_zone_ur_saves_percent` = "numeric",
    `gate_zone_ul_goals_against` = "integer", `gate_zone_ul_shots_against` = "integer", `gate_zone_ul_saves` = "integer",
    `gate_zone_ul_saves_percent` = "numeric", `gate_zone_br_goals_against` = "integer", `gate_zone_br_shots_against` = "integer",
    `gate_zone_br_saves` = "integer", `gate_zone_br_saves_percent` = "numeric", `gate_zone_fh_goals_against` = "integer",
    `gate_zone_fh_shots_against` = "integer", `gate_zone_fh_saves` = "integer", `gate_zone_fh_saves_percent` = "numeric",
    `gate_zone_bl_goals_against` = "integer", `gate_zone_bl_shots_against` = "integer", `gate_zone_bl_saves` = "integer",
    `gate_zone_bl_saves_percent` = "numeric", `gate_zone_ce_goals_against` = "integer", `gate_zone_ce_shots_against` = "integer",
    `gate_zone_ce_saves` = "integer", `gate_zone_ce_saves_percent` = "numeric", `rank` = "integer"
  ),
  metal_standings = c(
    `group_id` = "integer", `group_name` = "character", `complete` = "logical",
    `team_id` = "integer", `part_team_id` = "integer", `name` = "character",
    `team_original` = "character", `shortcut` = "character", `position` = "integer",
    `points` = "integer", `games` = "integer", `wins` = "integer",
    `wins_ot` = "integer", `wins_reg` = "integer", `ties` = "integer",
    `losts_ot` = "integer", `losts` = "integer", `score1` = "integer",
    `score2` = "integer", `goals_away` = "integer", `points_away` = "integer",
    `last10` = "character", `only_ot_wins` = "integer", `only_so_wins` = "integer",
    `only_ot_losts` = "integer", `only_so_losts` = "integer", `qualified` = "logical",
    `live` = "logical", `live_game_score` = "character", `position_change` = "integer",
    `ppg` = "numeric"
  ),
  metal_game_center = c(
    `status_id` = "integer", `status_msg` = "character", `data_game_data_id` = "integer",
    `data_game_data_division_id` = "integer", `data_game_data_division_longname` = "character", `data_game_data_division_shortname` = "character",
    `data_game_data_scheduled_date_formatted_short` = "character", `data_game_data_scheduled_date_formatted_long` = "character", `data_game_data_scheduled_date_timestamp` = "numeric",
    `data_game_data_scheduled_date_value` = "character", `data_game_data_scheduled_date_diff_to_now` = "integer", `data_game_data_scheduled_time` = "character",
    `data_game_data_start_time` = "character", `data_game_data_end_time` = "character", `data_game_data_is_overtime` = "integer",
    `data_game_data_is_shoot_out` = "integer", `data_game_data_home_team_id` = "integer", `data_game_data_home_team_longname` = "character",
    `data_game_data_home_team_shortname` = "character", `data_game_data_home_team_flavourname` = "character", `data_game_data_home_team_score` = "integer",
    `data_game_data_away_team_id` = "integer", `data_game_data_away_team_longname` = "character", `data_game_data_away_team_shortname` = "character",
    `data_game_data_away_team_flavourname` = "character", `data_game_data_away_team_score` = "integer", `data_game_data_game_status` = "integer",
    `data_game_data_game_extended_status` = "integer", `data_game_data_game_extended_status_info_text` = "character", `data_game_data_game_name` = "character",
    `data_game_data_game_day` = "character", `data_game_data_game_round` = "integer", `data_game_data_location_longname` = "character",
    `data_game_data_location_shortname` = "character", `data_game_data_location_coordinates_lat` = "integer", `data_game_data_location_coordinates_lng` = "integer",
    `data_game_data_attendance` = "integer", `data_game_data_live_time` = "integer", `data_game_data_live_time_formatted` = "character",
    `data_game_data_live_time_period` = "integer", `data_game_data_live_time_string` = "character", `data_game_data_live_time_game_phase` = "character",
    `data_game_data_period_stats` = "character", `data_game_data_game_officials_ref1_lastname` = "character", `data_game_data_game_officials_ref1_firstname` = "character",
    `data_game_data_game_officials_ref2_lastname` = "character", `data_game_data_game_officials_ref2_firstname` = "character", `data_game_data_game_officials_linesman1_lastname` = "character",
    `data_game_data_game_officials_linesman1_firstname` = "character", `data_game_data_game_officials_linesman2_lastname` = "character", `data_game_data_game_officials_linesman2_firstname` = "character",
    `data_game_data_team_officials_headcoachhome_lastname` = "character", `data_game_data_team_officials_headcoachhome_firstname` = "character", `data_game_data_team_officials_headcoachaway_lastname` = "character",
    `data_game_data_team_officials_headcoachaway_firstname` = "character", `data_game_data_live_time_period_bar` = "character", `data_game_data_you_tube_link` = "character",
    `data_game_data_number_of_periods` = "integer", `data_game_data_length_of_period` = "integer", `data_game_data_number_of_overtimes` = "integer",
    `data_game_data_length_of_overtime` = "integer", `data_game_data_shootout_shots` = "integer", `data_game_data_images_home_team_logo` = "character",
    `data_game_data_images_away_team_logo` = "character", `data_game_data_streams` = "character", `data_game_data_home_team_logo_url` = "character",
    `data_game_data_away_team_logo_url` = "character", `data_home_goals` = "character", `data_home_penalties` = "character",
    `data_home_field_players` = "character", `data_home_goal_keepers` = "character", `data_home_team_stats_faceoffs_won` = "integer",
    `data_home_team_stats_faceoffs_total` = "integer", `data_home_team_stats_shots_on_goal` = "integer", `data_home_team_stats_shots_blocked` = "integer",
    `data_home_team_stats_penalty_minutes` = "integer", `data_home_team_stats_powerplay_goals` = "integer", `data_home_team_stats_powerplay_opportunities` = "integer",
    `data_home_team_stats_penaltykill_successes` = "integer", `data_home_team_stats_penaltykill_situations` = "integer", `data_home_team_stats_time_values_leading` = "integer",
    `data_home_team_stats_time_values_tied` = "integer", `data_home_team_stats_time_values_trailing` = "integer", `data_home_team_stats_time_values_time_game_strength_even` = "integer",
    `data_home_team_stats_time_values_time_game_strength_plus_one` = "integer", `data_home_team_stats_time_values_time_game_strength_plus_two` = "integer", `data_home_team_stats_time_values_time_game_strength_minus_one` = "integer",
    `data_home_team_stats_time_values_time_game_strength_minus_two` = "integer", `data_home_team_stats_time_values_time_on_powerplay` = "integer", `data_home_team_stats_time_values_time_while_shorthanded` = "integer",
    `data_away_goals` = "character", `data_away_penalties` = "character", `data_away_field_players` = "character",
    `data_away_goal_keepers` = "character", `data_away_team_stats_faceoffs_won` = "integer", `data_away_team_stats_faceoffs_total` = "integer",
    `data_away_team_stats_shots_on_goal` = "integer", `data_away_team_stats_shots_blocked` = "integer", `data_away_team_stats_penalty_minutes` = "integer",
    `data_away_team_stats_powerplay_goals` = "integer", `data_away_team_stats_powerplay_opportunities` = "integer", `data_away_team_stats_penaltykill_successes` = "integer",
    `data_away_team_stats_penaltykill_situations` = "integer", `data_away_team_stats_time_values_leading` = "integer", `data_away_team_stats_time_values_tied` = "integer",
    `data_away_team_stats_time_values_trailing` = "integer", `data_away_team_stats_time_values_time_game_strength_even` = "integer", `data_away_team_stats_time_values_time_game_strength_plus_one` = "integer",
    `data_away_team_stats_time_values_time_game_strength_plus_two` = "integer", `data_away_team_stats_time_values_time_game_strength_minus_one` = "integer", `data_away_team_stats_time_values_time_game_strength_minus_two` = "integer",
    `data_away_team_stats_time_values_time_on_powerplay` = "integer", `data_away_team_stats_time_values_time_while_shorthanded` = "integer", `data_home_goal_keeper_changes` = "character",
    `data_away_goal_keeper_changes` = "character", `data_game_actions` = "character", `data_faceoffs_head_to_head` = "character",
    `data_faceoffs_individual` = "character", `data_game_situations` = "character", `calc_time` = "integer",
    `last_update_formatted_short` = "character", `last_update_formatted_long` = "character", `last_update_timestamp` = "numeric",
    `last_update_value` = "character", `served_by_server` = "character"
  )
)


# ---------------------------------------------------------------------------
# Parsers. Mirrors sdv-py hockey/esports_parsers.py. A body is decoded with
# simplifyVector = FALSE (objects are named lists, arrays unnamed lists, JSON
# null is NULL). Every parser turns a body into a list of "rows" (named lists)
# and `.es_frame()` types them against the route's returns schema. A parser
# never raises: an unusable body is a zero-row tibble with the documented
# columns.
# ---------------------------------------------------------------------------

# Wire keys the snake_case rule would split badly (xG -> x_g) or that two
# buckets spell differently.
.ES_KEYS <- c(
  xG = "xg", xGA = "xga", xS = "xs", GSAx = "gsax", xG_type = "xg_type",
  xGStory = "xg_story", homeD2W = "home_d2w", awayD2W = "away_d2w",
  onlyOTwins = "only_ot_wins", onlyOTlosts = "only_ot_losts",
  onlySOwins = "only_so_wins", onlySOlosts = "only_so_losts",
  losser = "loser", looser = "loser"
)
.ES_ID <- "(^|_)id$"
.ES_FLOAT <- paste0(
  "(percent|per_game|efficiency|average|^ppg$|(^|_)(xg|xga|xs|gsax)$|ex_goals|d2w|",
  "milestone|minutes_in_play|^coordinate_[xy]$|timeonice)"
)
# period-events keys that are not numbered periods: key -> label (period number is 4 / 5).
.ES_EXTRA_PERIODS <- c(overtime = "OT", shootout = "SO")
.ES_EXTRA_NUMBERS <- c(overtime = 4L, shootout = 5L)
# period-events `data` objects that hold one player -> column prefix.
.ES_PEOPLE <- c(
  scorer = "scorer", disciplinedPlayer = "penalized", replacedPlayer = "replaced",
  fouledPlayer = "fouled", player = "goalie", outgoingGoalkeeper = "outgoing_goalie"
)
.ES_EVENT_SCALARS <- c(id = "event_id", team = "team_side", ea = "extra_attacker", en = "empty_net")
# shot `match_shot_resutl_id` (sic) code of a goal; 1/2/3 are unverified.
.ES_GOAL_RESULT <- 4L

# parser behind each route
.ES_PARSERS <- c(
  schedule = "schedule", team_player_stats = "player_stats",
  league_part_player_stats = "player_stats", player_box = "player_stats",
  top_scorers = "generic", faceoffs = "generic", goalie_box = "generic",
  game_center = "generic", game_info = "game_info", pbp = "pbp",
  roster = "lineup", shifts = "shifts", standings = "standings", shots = "shots"
)

# ---- key and value helpers -------------------------------------------------

.es_underscore <- function(x) {
  x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", x, perl = TRUE)
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x, perl = TRUE)
  tolower(gsub("-", "_", x, fixed = TRUE))
}

# Wire key -> column name: `.ES_KEYS`, else snake_case with any other
# character run as "_" (`toi/gp` -> `toi_gp`).
.es_key <- function(k) {
  k <- as.character(k)
  hit <- .ES_KEYS[k]
  if (!is.na(hit)) return(unname(hit))
  gsub("^_+|_+$", "", gsub("(*UCP)\\W+", "_", .es_underscore(k), perl = TRUE))
}

# `row[name] <- value`, keeping a NULL value as a (null) column.
.es_set <- function(row, name, value) {
  row[name] <- list(value)
  row
}

.es_merge <- function(a, b) {
  for (nm in names(b)) a[nm] <- b[nm]
  a
}

.es_omit <- function(x, keys) x[!(names(x) %in% keys)]

.es_dicts <- function(x) {
  if (is.list(x) && is.null(names(x))) Filter(.esports_is_obj, x) else list()
}

# Nested object -> one flat row `<parent>_<child>` (an empty object is a null cell).
.es_flatten <- function(obj, prefix = "", out = list()) {
  nms <- names(obj)
  for (i in seq_along(obj)) {
    key <- .es_key(nms[i])
    name <- if (nzchar(prefix)) paste0(prefix, "_", key) else key
    v <- obj[[i]]
    if (.esports_is_obj(v) && length(v)) {
      out <- .es_flatten(v, name, out)
    } else {
      out <- .es_set(out, name, if (.esports_is_obj(v)) NULL else v)
    }
  }
  out
}

# Python's repr(float): the shortest digits that round-trip, `.0` on whole numbers.
.es_fmt_double <- function(x) {
  if (is.na(x)) return(NA_character_)
  if (x == floor(x) && abs(x) < 1e16) return(sprintf("%.1f", x))
  for (d in 1:17) {
    s <- format(x, digits = d, scientific = FALSE, trim = TRUE)
    if (as.numeric(s) == x) return(s)
  }
  format(x, digits = 17, trim = TRUE)
}

.es_dumps_str <- function(s) as.character(jsonlite::toJSON(s, auto_unbox = TRUE))

# json.dumps(v, ensure_ascii=False): the text of a list cell.
.es_dumps <- function(v) {
  if (is.null(v)) return("null")
  if (is.list(v)) {
    if (!is.null(names(v))) {
      if (!length(v)) return("{}")
      keys <- vapply(names(v), .es_dumps_str, "")
      vals <- vapply(v, .es_dumps, "")
      return(paste0("{", paste0(keys, ": ", vals, collapse = ", "), "}"))
    }
    return(paste0("[", paste(vapply(v, .es_dumps, ""), collapse = ", "), "]"))
  }
  if (is.logical(v)) return(if (is.na(v)) "null" else tolower(as.character(v)))
  if (is.integer(v)) return(as.character(v))
  # a whole double beyond 32 bits was a JSON integer (jsonlite reads it as a double)
  if (is.double(v)) return(if (!is.na(v) && v == floor(v) && abs(v) >= 2^31 && abs(v) < 1e15) sprintf("%.0f", v) else .es_fmt_double(v))
  .es_dumps_str(as.character(v))
}

.es_text <- function(v) if (is.character(v) && length(v) == 1) v else .es_dumps(v)

.es_truthy <- function(v) {
  if (is.null(v) || !length(v)) return(FALSE)
  if (is.list(v)) return(TRUE)
  if (is.na(v[[1]])) return(FALSE)
  if (is.character(v)) nzchar(v[[1]]) else v[[1]] != 0
}

# Python `==` on two JSON scalars: numbers compare as numbers, a string never
# equals a number, null equals only null.
.es_pyeq <- function(a, b) {
  if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
  num <- function(x) (is.integer(x) || is.double(x) || is.logical(x)) && length(x) == 1
  if (num(a) && num(b)) return(isTRUE(as.numeric(a) == as.numeric(b)))
  if (is.character(a) && is.character(b)) return(identical(a, b))
  FALSE
}

# ---- typing -----------------------------------------------------------------

.es_schema <- function(league, route) .ES_SCHEMAS[[paste0(league, "_", route)]]

# returns-schema type -> dtype tag: integer, double, logical, character
.es_tags <- function(schema) {
  tag <- ifelse(schema %in% c("numeric", "double"), "double",
         ifelse(schema == "integer", "integer",
         ifelse(schema == "logical", "logical", "character")))
  names(tag) <- names(schema)
  tag
}

.es_cache <- new.env(parent = emptyenv())

# column -> dtype tag where every schema of every league agrees
.es_by_name <- function() {
  if (is.null(.es_cache$by_name)) {
    all <- unlist(unname(lapply(.ES_SCHEMAS, .es_tags)))
    agreed <- tapply(unname(all), names(all), function(t) if (length(unique(t)) == 1) t[[1]] else NA_character_)
    agreed <- agreed[!is.na(agreed)]
    .es_cache$by_name <- structure(as.character(agreed), names = names(agreed))
  }
  .es_cache$by_name
}

.es_lookup <- function(x, name) {
  hit <- x[name]
  if (is.na(hit)) NA_character_ else unname(hit)
}

# dtype of an all-null column: its route's schema, else every schema's, else a name rule
.es_empty_dtype <- function(name, types) {
  d <- .es_lookup(types, name)
  if (is.na(d)) d <- .es_lookup(.es_by_name(), name)
  if (!is.na(d)) return(d)
  if (grepl(.ES_ID, name, perl = TRUE) || endsWith(name, "_count")) return("integer")
  if (grepl(.ES_FLOAT, name, perl = TRUE)) "double" else "character"
}

.es_kind <- function(v) {
  if (is.null(v)) return(NA_character_)
  if (is.list(v)) return("other")
  if (is.logical(v)) "logical" else if (is.integer(v)) "integer" else if (is.double(v)) "double" else "character"
}

# One column with an explicit type, so a heterogeneous body never raises.
.es_series <- function(vals) {
  kinds <- unique(vapply(vals, .es_kind, ""))
  kinds <- kinds[!is.na(kinds)]
  n <- length(vals)
  if (!length(kinds)) return(list(dt = "null", x = rep(NA, n)))
  pick <- function(f, template) vapply(vals, function(v) if (is.null(v)) template else f(v), template)
  if (identical(kinds, "logical")) {
    return(list(dt = "logical", x = pick(identity, NA)))
  }
  if (identical(kinds, "integer")) {
    return(list(dt = "integer", x = pick(identity, NA_integer_)))
  }
  if (all(kinds %in% c("integer", "double"))) {
    return(list(dt = "double", x = pick(as.double, NA_real_)))
  }
  list(dt = "character", x = pick(.es_text, NA_character_))
}

.es_cast <- function(x, from, to) {
  if (from == to) return(x)
  switch(to,
    double = as.double(x),
    integer = as.integer(x),
    logical = as.logical(x),
    character = switch(from,
      double = vapply(x, function(z) if (is.na(z)) NA_character_ else .es_fmt_double(z), ""),
      logical = ifelse(is.na(x), NA_character_, tolower(as.character(x))),
      as.character(x)
    )
  )
}

# src casts to the schema dtype without losing a value
.es_widens <- function(src, dst) {
  (src == "integer" && dst == "double") ||
    (dst == "character" && src %in% c("integer", "double", "logical"))
}

# String id column -> integer when every value survives the cast (never "007", never 1.5)
.es_ids_integer <- function(x) {
  ok <- !is.na(x)
  if (!all(grepl("^-?[0-9]+$", x[ok]))) return(NULL)
  num <- suppressWarnings(as.numeric(x[ok]))
  if (anyNA(num) || any(format(num, scientific = FALSE, trim = TRUE) != x[ok])) return(NULL)
  out <- rep(NA_real_, length(x))
  out[ok] <- num
  if (all(abs(out[ok]) <= .Machine$integer.max)) as.integer(out) else out
}

# Rows -> tibble with the route's schema (documented columns first and in order;
# a documented column the body does not send is a null column of its schema type;
# undocumented columns follow).
.es_frame <- function(rows, league, route) {
  types <- .es_tags(.es_schema(league, route))
  if (!length(rows)) return(.empty_tibble(types))
  nms <- unique(unlist(lapply(rows, names)))
  cols <- list()
  for (n in nms) {
    s <- .es_series(lapply(rows, function(r) r[[n]]))
    x <- s$x
    dt <- s$dt
    want <- .es_lookup(types, n)
    if (is.na(want)) want <- .es_lookup(.es_by_name(), n)
    if (dt == "null") {
      to <- .es_empty_dtype(n, types)
      x <- .es_cast(x, "null", to)
      dt <- to
    } else if (!is.na(want) && want != dt && .es_widens(dt, want)) {
      x <- .es_cast(x, dt, want)
      dt <- want
    } else if (dt == "integer" && grepl(.ES_FLOAT, n, perl = TRUE) && !grepl(.ES_ID, n, perl = TRUE)) {
      x <- as.double(x)
      dt <- "double"
    }
    if (dt == "character" && grepl(.ES_ID, n, perl = TRUE)) {
      as_int <- .es_ids_integer(x)
      if (!is.null(as_int)) x <- as_int
    }
    cols[[n]] <- x
  }
  documented <- names(types)
  for (n in setdiff(documented, nms)) {
    cols[[n]] <- .es_cast(rep(NA, length(rows)), "null", .es_empty_dtype(n, types))
  }
  tibble::as_tibble(cols[c(documented, setdiff(names(cols), documented))])
}

# ---- row builders -----------------------------------------------------------

# league-matches {"matches": [...]}: `id` -> match_id; season re-keyed to the END
# year; `results_score_` / `results_` prefixes dropped; DEL's BOM in `round` stripped.
.es_rows_schedule <- function(raw) {
  matches <- if (.esports_is_obj(raw)) raw[["matches"]] else raw
  bom <- intToUtf8(0xFEFF)
  lapply(.es_dicts(matches), function(m) {
    row <- list()
    flat <- .es_flatten(m)
    for (k in names(flat)) {
      v <- flat[[k]]
      name <- if (k == "id") "match_id" else sub("^results_", "", sub("^results_score_", "", k))
      if (name == "season") {
        v <- if ((is.integer(v) || is.character(v)) && length(v) == 1 && grepl("^[0-9]+$", as.character(v))) {
          as.integer(v) + 1L
        } else {
          NULL
        }
      } else if (name == "round" && is.character(v)) {
        v <- gsub(bom, "", v, fixed = TRUE)
      }
      row <- .es_set(row, name, v)
    }
    row
  })
}

# {playerId, jersey, name, surname} -> <prefix>_player_id / _jersey / _name
.es_person <- function(prefix, p) {
  if (!.esports_is_obj(p)) return(list())
  parts <- Filter(.es_truthy, list(p[["name"]], p[["surname"]]))
  name <- paste(vapply(parts, .es_text, ""), collapse = " ")
  out <- list()
  out <- .es_set(out, paste0(prefix, "_player_id"), p[["playerId"]])
  out <- .es_set(out, paste0(prefix, "_jersey"), p[["jersey"]])
  .es_set(out, paste0(prefix, "_name"), if (nzchar(name)) name else NULL)
}

.es_event_data <- function(data) {
  out <- list()
  if (!.esports_is_obj(data)) return(out)
  for (k in names(data)) {
    v <- data[[k]]
    if (k == "time") {
      # a penalty's {from, to}{realTime, scoreboardTime}; else the event time again
      if (.esports_is_obj(v)) out <- .es_merge(out, .es_flatten(v, "penalty"))
    } else if (k == "assistants") {
      i <- 0L
      for (p in .es_dicts(v)) {
        i <- i + 1L
        out <- .es_merge(out, .es_person(paste0("assist", i), p))
      }
    } else if (k == "attendants") {
      for (side in c("positive", "negative")) {
        label <- if (side == "positive") "plus" else "minus"
        people <- if (.esports_is_obj(v)) .es_dicts(v[[side]]) else list()
        ids <- vapply(people, function(p) {
          x <- p[["playerId"]]
          if (is.null(x)) "None" else .es_text(x)
        }, "")
        out <- .es_set(out, paste0(label, "_player_ids"), if (length(ids)) paste(ids, collapse = ",") else NULL)
      }
    } else if (k %in% names(.ES_PEOPLE)) {
      out <- .es_merge(out, .es_person(.ES_PEOPLE[[k]], v))
    } else if (.esports_is_obj(v)) {
      out <- .es_merge(out, .es_flatten(v, .es_key(k)))
    } else {
      name <- if (k %in% names(.ES_EVENT_SCALARS)) .ES_EVENT_SCALARS[[k]] else .es_key(k)
      out <- .es_set(out, name, v)
    }
  }
  out
}

# period-events keys holding event arrays: numbered periods in order, then the rest by name
.es_period_keys <- function(raw) {
  ks <- names(raw)[vapply(raw, function(v) is.list(v) && is.null(names(v)), NA)]
  dig <- grepl("^[0-9]+$", ks)
  c(ks[dig][order(as.numeric(ks[dig]))], sort(ks[!dig], method = "radix"))
}

# {"1": [...], "2": [...], "3": [...], "overtime": [...], "shootout": [...]}: one row per event
.es_rows_pbp <- function(raw) {
  rows <- list()
  if (!.esports_is_obj(raw)) return(rows)
  for (key in .es_period_keys(raw)) {
    if (grepl("^[0-9]+$", key)) {
      label <- key
      number <- as.integer(key)
    } else {
      label <- if (key %in% names(.ES_EXTRA_PERIODS)) .ES_EXTRA_PERIODS[[key]] else key
      number <- if (key %in% names(.ES_EXTRA_NUMBERS)) .ES_EXTRA_NUMBERS[[key]] else NULL
    }
    for (event in .es_dicts(raw[[key]])) {
      tm <- event[["time"]]
      # DEL writes periodEnd times as floats (1200.0); a game second is an integer
      if (is.double(tm) && length(tm) == 1 && !is.na(tm) && tm == floor(tm)) tm <- as.integer(tm)
      row <- list()
      row <- .es_set(row, "event_index", length(rows))
      row <- .es_set(row, "period", label)
      row <- .es_set(row, "period_number", number)
      row <- .es_set(row, "time", tm)
      row <- .es_set(row, "event_type", event[["type"]])
      row <- .es_merge(row, .es_event_data(event[["data"]]))
      rows[[length(rows) + 1L]] <- row
    }
  }
  rows
}

# Players with a nested `statistics`: `id` -> player_id; a stat split by venue
# ({home, away}) gives its total under the plain name plus <name>_home / _away.
.es_rows_player_stats <- function(raw) {
  lapply(.es_dicts(raw), function(player) {
    row <- .es_set(list(), "player_id", player[["id"]])
    row <- .es_merge(row, .es_flatten(.es_omit(player, c("id", "statistics"))))
    stats <- player[["statistics"]]
    if (.esports_is_obj(stats)) {
      for (k in names(stats)) {
        v <- stats[[k]]
        name <- .es_key(k)
        is_num <- function(x) (is.integer(x) || is.double(x)) && length(x) == 1
        if (.esports_is_obj(v) && length(v) == 2 && setequal(names(v), c("home", "away")) &&
            is_num(v[["home"]]) && is_num(v[["away"]])) {
          row <- .es_set(row, name, v[["home"]] + v[["away"]])
          row <- .es_set(row, paste0(name, "_home"), v[["home"]])
          row <- .es_set(row, paste0(name, "_away"), v[["away"]])
        } else {
          row <- .es_merge(row, .es_flatten(structure(list(v), names = k)))
        }
      }
    }
    row
  })
}

# visualization/shots {"match": {..., "shots": [...]}}: one row per shot with the
# match's scalar fields on each row; `team_side` from team_id; `is_goal` = result code 4.
.es_rows_shots <- function(raw) {
  match <- if (.esports_is_obj(raw)) raw[["match"]] else NULL
  if (!.esports_is_obj(match)) match <- list()
  context <- .es_set(list(), "match_id", match[["id"]])
  for (k in names(match)) {
    if (k != "id" && !is.list(match[[k]])) context <- .es_set(context, .es_key(k), match[[k]])
  }
  lapply(.es_dicts(match[["shots"]]), function(shot) {
    row <- context
    flat <- .es_flatten(shot)
    for (k in names(flat)) {
      v <- flat[[k]]
      if (k == "polygon" && !is.character(v)) v <- NULL
      row <- .es_set(row, if (k == "id") "shot_id" else k, v)
    }
    code <- row[["match_shot_resutl_id"]]
    row <- .es_set(row, "shot_result_id", code)
    row <- .es_set(row, "is_goal", if (is.null(code)) NULL else .es_pyeq(code, .ES_GOAL_RESULT))
    team <- row[["team_id"]]
    side <- if (.es_pyeq(team, context[["home_id"]])) {
      "home"
    } else if (.es_pyeq(team, context[["visitor_id"]])) {
      "visitor"
    } else {
      NULL
    }
    row <- .es_set(row, "team_side", side)
    if (!"xg" %in% names(row)) row <- .es_set(row, "xg", NULL)
    if (!"xg_type" %in% names(row)) row <- .es_set(row, "xg_type", NULL)
    row
  })
}

# tables / table: DEL [{leaguePartId, tableData: {table, ...}}] or
# [{groupId, groupName, complete, clubs}]; one row per team, `id` -> team_id.
.es_rows_standings <- function(raw) {
  rows <- list()
  for (block in .es_dicts(raw)) {
    data <- block[["tableData"]]
    if (.esports_is_obj(data)) {
      context <- .es_flatten(.es_omit(block, "tableData"))
      context <- .es_merge(context, .es_flatten(.es_omit(data, "table")))
      teams <- .es_dicts(data[["table"]])
    } else {
      context <- .es_flatten(.es_omit(block, "clubs"))
      teams <- .es_dicts(block[["clubs"]])
    }
    for (team in teams) {
      row <- context
      flat <- .es_flatten(team)
      for (k in names(flat)) row <- .es_set(row, if (k == "id") "team_id" else k, flat[[k]])
      rows[[length(rows) + 1L]] <- row
    }
  }
  rows
}

# DEL shiftsSC: one row per shift, `duration` = end - start (game seconds).
.es_rows_shifts <- function(raw) {
  rename <- c(
    id = "shift_id", start_time_time = "start_time", start_time_realtime = "start_real_time",
    end_time_time = "end_time", end_time_realtime = "end_real_time"
  )
  lapply(.es_dicts(raw), function(shift) {
    row <- list()
    flat <- .es_flatten(shift)
    for (k in names(flat)) row <- .es_set(row, if (k %in% names(rename)) rename[[k]] else k, flat[[k]])
    s <- row[["start_time"]]
    e <- row[["end_time"]]
    .es_set(row, "duration", if (is.integer(s) && is.integer(e)) e - s else NULL)
  })
}

# {home: {slot: player}, visitor: {...}} (DEL roster.json, or the `roster` of a
# game-header / game-info body): one row per player, home then visitor, slot order.
.es_rows_lineup <- function(raw) {
  roster <- if (.esports_is_obj(raw) && .esports_is_obj(raw[["roster"]])) raw[["roster"]] else raw
  rows <- list()
  for (side in c("home", "visitor")) {
    team <- if (.esports_is_obj(roster)) roster[[side]] else NULL
    if (!.esports_is_obj(team)) next
    for (slot in sort(names(team), method = "radix")) {
      player <- team[[slot]]
      if (.esports_is_obj(player)) {
        row <- list(team_side = side, slot = slot)
        rows[[length(rows) + 1L]] <- .es_merge(row, .es_flatten(.es_omit(player, "roster")))
      }
    }
  }
  rows
}

# game-header / game-info: one row of game info, the lineup left out.
.es_rows_game_info <- function(raw) {
  info <- NULL
  if (.esports_is_obj(raw)) {
    info <- if (.esports_is_obj(raw[["gameInfo"]])) raw[["gameInfo"]] else .es_omit(raw, "roster")
  }
  if (length(info)) list(.es_flatten(info)) else list()
}

# top goalies, top scorers, faceoffs, the widget bundle: a list is one row per
# object, any other object is one row.
.es_rows_generic <- function(raw) {
  if (is.list(raw) && is.null(names(raw))) {
    lapply(.es_dicts(raw), .es_flatten)
  } else if (.esports_is_obj(raw) && length(raw)) {
    list(.es_flatten(raw))
  } else {
    list()
  }
}

# Body -> tibble for `route` of `league`, typed by the route's returns schema.
.es_parse <- function(route, raw, league) {
  rows <- switch(.ES_PARSERS[[route]],
    schedule = .es_rows_schedule(raw),
    player_stats = .es_rows_player_stats(raw),
    generic = .es_rows_generic(raw),
    game_info = .es_rows_game_info(raw),
    pbp = .es_rows_pbp(raw),
    lineup = .es_rows_lineup(raw),
    shifts = .es_rows_shifts(raw),
    standings = .es_rows_standings(raw),
    shots = .es_rows_shots(raw)
  )
  .es_frame(rows, league, route)
}


# ---------------------------------------------------------------------------
# Per-route cores: .esports_<route>(league, ...) fetch one bucket key through
# .esports_get() and parse it. `return_parsed = FALSE` returns the raw decoded
# body. Options are scoped like every fastRhockey loader.
# ---------------------------------------------------------------------------

#' @noRd
.esports_schedule <- function(league, season, league_id = NULL, return_parsed = TRUE) {
  old <- options(list(scipen = 999, stringsAsFactors = FALSE))
  on.exit(options(old), add = TRUE)
  raw <- .esports_get(league, "schedule", season = season, league_id = league_id)
  if (!isTRUE(return_parsed)) return(raw)
  type <- paste(.esports_leagues()[[league]]$name, "schedule", "from the eSports hokejovyzapis bucket")
  make_fastRhockey_data(.es_parse("schedule", raw, league), type, Sys.time())
}

#' @noRd
.esports_team_player_stats <- function(league, season, team_id, league_id = NULL, return_parsed = TRUE) {
  old <- options(list(scipen = 999, stringsAsFactors = FALSE))
  on.exit(options(old), add = TRUE)
  raw <- .esports_get(league, "team_player_stats", season = season, team_id = team_id, league_id = league_id)
  if (!isTRUE(return_parsed)) return(raw)
  type <- paste(.esports_leagues()[[league]]$name, "team_player_stats", "from the eSports hokejovyzapis bucket")
  make_fastRhockey_data(.es_parse("team_player_stats", raw, league), type, Sys.time())
}

#' @noRd
.esports_top_scorers <- function(league, season, league_id = NULL, return_parsed = TRUE) {
  old <- options(list(scipen = 999, stringsAsFactors = FALSE))
  on.exit(options(old), add = TRUE)
  raw <- .esports_get(league, "top_scorers", season = season, league_id = league_id)
  if (!isTRUE(return_parsed)) return(raw)
  type <- paste(.esports_leagues()[[league]]$name, "top_scorers", "from the eSports hokejovyzapis bucket")
  make_fastRhockey_data(.es_parse("top_scorers", raw, league), type, Sys.time())
}

#' @noRd
.esports_league_part_player_stats <- function(league, league_part_id, return_parsed = TRUE) {
  old <- options(list(scipen = 999, stringsAsFactors = FALSE))
  on.exit(options(old), add = TRUE)
  raw <- .esports_get(league, "league_part_player_stats", league_part_id = league_part_id)
  if (!isTRUE(return_parsed)) return(raw)
  type <- paste(.esports_leagues()[[league]]$name, "league_part_player_stats", "from the eSports hokejovyzapis bucket")
  make_fastRhockey_data(.es_parse("league_part_player_stats", raw, league), type, Sys.time())
}

#' @noRd
.esports_faceoffs <- function(league, match_id, return_parsed = TRUE) {
  old <- options(list(scipen = 999, stringsAsFactors = FALSE))
  on.exit(options(old), add = TRUE)
  raw <- .esports_get(league, "faceoffs", match_id = match_id)
  if (!isTRUE(return_parsed)) return(raw)
  type <- paste(.esports_leagues()[[league]]$name, "faceoffs", "from the eSports hokejovyzapis bucket")
  make_fastRhockey_data(.es_parse("faceoffs", raw, league), type, Sys.time())
}

#' @noRd
.esports_game_info <- function(league, match_id, return_parsed = TRUE) {
  old <- options(list(scipen = 999, stringsAsFactors = FALSE))
  on.exit(options(old), add = TRUE)
  raw <- .esports_get(league, "game_info", match_id = match_id)
  if (!isTRUE(return_parsed)) return(raw)
  type <- paste(.esports_leagues()[[league]]$name, "game_info", "from the eSports hokejovyzapis bucket")
  make_fastRhockey_data(.es_parse("game_info", raw, league), type, Sys.time())
}

#' @noRd
.esports_pbp <- function(league, match_id, return_parsed = TRUE) {
  old <- options(list(scipen = 999, stringsAsFactors = FALSE))
  on.exit(options(old), add = TRUE)
  raw <- .esports_get(league, "pbp", match_id = match_id)
  if (!isTRUE(return_parsed)) return(raw)
  type <- paste(.esports_leagues()[[league]]$name, "pbp", "from the eSports hokejovyzapis bucket")
  make_fastRhockey_data(.es_parse("pbp", raw, league), type, Sys.time())
}

#' @noRd
.esports_roster <- function(league, match_id, return_parsed = TRUE) {
  old <- options(list(scipen = 999, stringsAsFactors = FALSE))
  on.exit(options(old), add = TRUE)
  raw <- .esports_get(league, "roster", match_id = match_id)
  if (!isTRUE(return_parsed)) return(raw)
  type <- paste(.esports_leagues()[[league]]$name, "roster", "from the eSports hokejovyzapis bucket")
  make_fastRhockey_data(.es_parse("roster", raw, league), type, Sys.time())
}

#' @noRd
.esports_shifts <- function(league, match_id, return_parsed = TRUE) {
  old <- options(list(scipen = 999, stringsAsFactors = FALSE))
  on.exit(options(old), add = TRUE)
  raw <- .esports_get(league, "shifts", match_id = match_id)
  if (!isTRUE(return_parsed)) return(raw)
  type <- paste(.esports_leagues()[[league]]$name, "shifts", "from the eSports hokejovyzapis bucket")
  make_fastRhockey_data(.es_parse("shifts", raw, league), type, Sys.time())
}

#' @noRd
.esports_player_box <- function(league, match_id, team_id, return_parsed = TRUE) {
  old <- options(list(scipen = 999, stringsAsFactors = FALSE))
  on.exit(options(old), add = TRUE)
  raw <- .esports_get(league, "player_box", match_id = match_id, team_id = team_id)
  if (!isTRUE(return_parsed)) return(raw)
  type <- paste(.esports_leagues()[[league]]$name, "player_box", "from the eSports hokejovyzapis bucket")
  make_fastRhockey_data(.es_parse("player_box", raw, league), type, Sys.time())
}

#' @noRd
.esports_goalie_box <- function(league, match_id, return_parsed = TRUE) {
  old <- options(list(scipen = 999, stringsAsFactors = FALSE))
  on.exit(options(old), add = TRUE)
  raw <- .esports_get(league, "goalie_box", match_id = match_id)
  if (!isTRUE(return_parsed)) return(raw)
  type <- paste(.esports_leagues()[[league]]$name, "goalie_box", "from the eSports hokejovyzapis bucket")
  make_fastRhockey_data(.es_parse("goalie_box", raw, league), type, Sys.time())
}

#' @noRd
.esports_standings <- function(league, season, league_id = NULL, return_parsed = TRUE) {
  old <- options(list(scipen = 999, stringsAsFactors = FALSE))
  on.exit(options(old), add = TRUE)
  raw <- .esports_get(league, "standings", season = season, league_id = league_id)
  if (!isTRUE(return_parsed)) return(raw)
  type <- paste(.esports_leagues()[[league]]$name, "standings", "from the eSports hokejovyzapis bucket")
  make_fastRhockey_data(.es_parse("standings", raw, league), type, Sys.time())
}

#' @noRd
.esports_shots <- function(league, match_id, return_parsed = TRUE) {
  old <- options(list(scipen = 999, stringsAsFactors = FALSE))
  on.exit(options(old), add = TRUE)
  raw <- .esports_get(league, "shots", match_id = match_id)
  if (!isTRUE(return_parsed)) return(raw)
  type <- paste(.esports_leagues()[[league]]$name, "shots", "from the eSports hokejovyzapis bucket")
  make_fastRhockey_data(.es_parse("shots", raw, league), type, Sys.time())
}

#' @noRd
.esports_game_center <- function(league, match_id, return_parsed = TRUE) {
  old <- options(list(scipen = 999, stringsAsFactors = FALSE))
  on.exit(options(old), add = TRUE)
  raw <- .esports_get(league, "game_center", match_id = match_id)
  if (!isTRUE(return_parsed)) return(raw)
  type <- paste(.esports_leagues()[[league]]$name, "game_center", "from the eSports hokejovyzapis bucket")
  make_fastRhockey_data(.es_parse("game_center", raw, league), type, Sys.time())
}


# ---------------------------------------------------------------------------
# Exported wrappers
# ---------------------------------------------------------------------------

#' @title **DEL Schedule**
#' @description Schedule and results of one competition season, one row per match, played or not.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/league-matches/{season}/{league_id}.json` of the DEL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/league-matches/2026/1.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param league_id Competition id. 1 = DEL regular season, 3 = DEL playoffs (also 2 = Test-Liga pre-season, 4 = MagentaSport Cup 2020/21, 5 = NHL Global Series 2022/23).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | match_id | integer | Match id (per-bucket match key space); joins to the `{match_id}` path param of the game wrappers. |
#'    | start_date | character | Scheduled face-off, local `YYYY-MM-DD HH:MM:SS`; DEL playoff placeholders read `0000-00-00 ...`. |
#'    | real_end_date | character | Actual end, local `YYYY-MM-DD HH:MM:SS`. |
#'    | league_id | integer | League id (per-bucket competition key space); joins to the `{league_id}` path param. |
#'    | league | character | League display name, e.g. `DEL`, `EIHL`, `Metal Ligaen`. |
#'    | season | integer | Season by its END year (2027 = 2026/27); the body's START-year key plus one. |
#'    | round | character | Round number as a string (`"1"`, `"249"`); DEL values carry a leading U+FEFF byte-order mark, empty string in Denmark. |
#'    | home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | home_name | character | Team display name. |
#'    | home_shortcut | character | Three-letter team code. |
#'    | guest_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | guest_name | character | Team display name. |
#'    | guest_shortcut | character | Three-letter team code. |
#'    | status | character | Match status: `BEFORE_MATCH`, `LIVE`, `AFTER_MATCH`. |
#'    | overtime_count | integer | Number of overtime periods played; null in the samples. |
#'    | extra_time | logical | True when the game went to overtime. |
#'    | shooting | logical | True when the game was decided by a shootout. |
#'    | contumacy | logical | True when the result was awarded by forfeit. |
#'    | final_score_home | integer | Home team goals in that span. |
#'    | final_score_guest | integer | Visiting team goals in that span. |
#'    | first_period_score_home | integer | Home team goals in that span. |
#'    | first_period_score_guest | integer | Visiting team goals in that span. |
#'    | second_period_score_home | integer | Home team goals in that span. |
#'    | second_period_score_guest | integer | Visiting team goals in that span. |
#'    | third_period_score_home | integer | Home team goals in that span. |
#'    | third_period_score_guest | integer | Visiting team goals in that span. |
#'    | actual_time_name | character | Display label of the game state in the bucket's language, e.g. `end`, `Ende`, `before game`. |
#'    | actual_time_alias | character | Short game-state code: `K` = finished (Czech `konec`), `0` = not started. |
#'    | last_event_time | integer | Game second of the latest event, of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | overtime_score_home | integer | Home team goals in overtime (present once a game of the season went to overtime). |
#'    | overtime_score_guest | integer | Visiting team goals in overtime (present once a game of the season went to overtime). |
#'    | shootout_score_home | integer | Home team shootout goals (present once a game of the season went to a shootout). |
#'    | shootout_score_guest | integer | Visiting team shootout goals (present once a game of the season went to a shootout). |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [DEL](https://www.penny-del.org/) (the league site whose widgets read this public eSports bucket)
#' @keywords DEL eSports
#' @family DEL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(del_schedule(season = 2027, league_id = 1))
#' }
del_schedule <- function(season, league_id = 1L, return_parsed = TRUE) {
  .esports_schedule("del", season = season, league_id = league_id, return_parsed = return_parsed)
}

#' @title **DEL Team Player Stats**
#' @description Season player stats of one team in one competition, one row per player.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/league-team-stats/{season}/{league_id}/{team_id}.json` of the DEL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/league-team-stats/2026/1/12.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param team_id Team id (per-bucket team key space: home_id / guest_id in del_schedule).
#' @param league_id Competition id. 1 = DEL regular season, 3 = DEL playoffs (also 2 = Test-Liga pre-season, 4 = MagentaSport Cup 2020/21, 5 = NHL Global Series 2022/23).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (playerId key space, per bucket, stable across games and seasons). |
#'    | holema_id | integer | Unobserved (null in every sample); an external registry id by its name, key space unknown, joins to nothing captured. |
#'    | name | character | Player full name. |
#'    | firstname | character | First name of the player in this row. |
#'    | surname | character | Player last name. |
#'    | date_of_birth | character | Birth date `YYYY-MM-DD`. |
#'    | active | logical | True when the player is active on the team. |
#'    | position | character | Position code: `GK` goalie, `DE` defence, `FW` / `FO` forward (FO meaning unverified). |
#'    | stick | character | Handedness: `left` or `right`; null when unknown. |
#'    | weight | integer | Weight in kg; `0` placeholder when unknown. |
#'    | height | integer | Height in cm; `0` placeholder when unknown. |
#'    | jersey | integer | Jersey number. |
#'    | birth_country | character | Player's country of birth (name). |
#'    | nationality | character | Nationality, country name. |
#'    | nationality_short | character | Nationality, three-letter code (`CAN`, `GER`; `UK` for Britain). |
#'    | team_shortcut | character | Three-letter team code. |
#'    | games | integer | Games played. |
#'    | goals | integer | goals in total (goals_home + goals_away: the body splits it by home and away games). |
#'    | goals_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | goals_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists | integer | assists in total (assists_home + assists_away: the body splits it by home and away games). |
#'    | assists_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points | integer | points in total (points_home + points_away: the body splits it by home and away games). |
#'    | points_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | penalty_minutes | integer | Penalty minutes assessed to the player (this game, or to date in the season / league-part files). |
#'    | positive | integer | Plus: on ice for goals for at even strength or shorthanded. |
#'    | negative | integer | Minus: on ice for goals against at even strength or on the power play. |
#'    | points_per_game | numeric | Average goals plus assists per game played. |
#'    | shot_efficiency | numeric | Shooting percentage, goals / shots on goal x 100. |
#'    | penalty_minutes_per_game | numeric | Average penalty minutes per game played. |
#'    | pp_goals | integer | Power-play goals. |
#'    | sh_goals | integer | Shorthanded goals. |
#'    | gw_goals | integer | Game-winning goals. |
#'    | shots_attempts | integer | Shot attempts (on goal + missed + blocked). |
#'    | shots_on_goal | integer | shots_on_goal in total (shots_on_goal_home + shots_on_goal_away: the body splits it by home and away games). |
#'    | shots_on_goal_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_on_goal_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_missed | integer | This player's shot attempts that missed the net. |
#'    | shots_blocked | integer | This player's shot attempts that were blocked. |
#'    | blocked_shots_by_player | integer | Opponent shots this player blocked. |
#'    | faceoffs_win | integer | Face-offs the player won (this game, or to date in the season / league-part files). |
#'    | faceoffs_losses | integer | Face-offs the player lost (this game, or to date in the season / league-part files). |
#'    | faceoffs_count | integer | Face-offs the player took (this game, or to date in the season / league-part files). |
#'    | time_on_ice | integer | Time on ice, whole seconds (`0` when not tracked). |
#'    | time_on_ice_pp | integer | Time on ice on the power play (`PP`) or shorthanded (`SH`), whole seconds. |
#'    | time_on_ice_sh | integer | Time on ice on the power play (`PP`) or shorthanded (`SH`), whole seconds. |
#'    | time_on_ice_per_game | numeric | Time on ice per game, seconds. |
#'    | shifts | integer | Shifts the player took (this game, or to date in the season / league-part files). |
#'    | shifts_per_game | numeric | Average shifts per game played. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [DEL](https://www.penny-del.org/) (the league site whose widgets read this public eSports bucket)
#' @keywords DEL eSports
#' @family DEL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(del_team_player_stats(season = 2027, team_id = 12, league_id = 1))
#' }
del_team_player_stats <- function(season, team_id, league_id = 1L, return_parsed = TRUE) {
  .esports_team_player_stats("del", season = season, team_id = team_id, league_id = league_id, return_parsed = return_parsed)
}

#' @title **DEL Top Scorers**
#' @description Skater scoring leaderboard of one competition season.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/league-top-scorers/{season}/{league_id}.json` of the DEL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/league-top-scorers/2026/1.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param league_id Competition id. 1 = DEL regular season, 3 = DEL playoffs (also 2 = Test-Liga pre-season, 4 = MagentaSport Cup 2020/21, 5 = NHL Global Series 2022/23).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | name | character | Player first name. |
#'    | surname | character | Player last name. |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_name | character | Team display name. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | goals | integer | Goals the player scored in the league-season. |
#'    | assists | integer | Assists credited to the player in the league-season. |
#'    | points | integer | Points, goals + assists. |
#'    | shots | integer | Shots on goal. |
#'    | games | integer | Games played (two names for the same count). |
#'    | penalty_minutes | integer | Penalty minutes assessed to the player in the league-season. |
#'    | time_on_ice | integer | Time on ice, seconds; `0` placeholder in the samples. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [DEL](https://www.penny-del.org/) (the league site whose widgets read this public eSports bucket)
#' @keywords DEL eSports
#' @family DEL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(del_top_scorers(season = 2027, league_id = 1))
#' }
del_top_scorers <- function(season, league_id = 1L, return_parsed = TRUE) {
  .esports_top_scorers("del", season = season, league_id = league_id, return_parsed = return_parsed)
}

#' @title **DEL Faceoffs**
#' @description Every faceoff of one match: time, dot, winner and loser.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/{match_id}/faceoffs.json` of the DEL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/4444/faceoffs.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in del_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | id | integer | Face-off id (face-off key space); no other captured file joins to it. |
#'    | time | integer | Seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | realtime | character | Wall-clock local time, `YYYY-MM-DD HH:MM:SS`. |
#'    | position | character | Face-off dot: `BULY_CENTRAL` or `BULY_{HOME/AWAY}_{DEFENSE/BLUE}_{LEFT/RIGHT}`. |
#'    | position_shortcut | character | Short dot code, e.g. `C`, `HDL` (home defence left), `HDR`. |
#'    | winner_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | winner_name | character | Player full name. |
#'    | loser_id | integer | Player id (playerId key space) of the faceoff loser (DEL spells the key losser, ICEHL looser). |
#'    | loser_name | character | Full name of the faceoff loser. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [DEL](https://www.penny-del.org/) (the league site whose widgets read this public eSports bucket)
#' @keywords DEL eSports
#' @family DEL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(del_faceoffs(match_id = 4444))
#' }
del_faceoffs <- function(match_id, return_parsed = TRUE) {
  .esports_faceoffs("del", match_id = match_id, return_parsed = return_parsed)
}

#' @title **DEL Game Info**
#' @description Game info of one match: state, arena, attendance, officials, coaches, period scores.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/{match_id}/game-header.json` of the DEL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/4444/game-header.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in del_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | actual_time_name | character | Display label of the game state, e.g. `end` / `Ende`. |
#'    | actual_time_alias | character | Short game-state code: `K` = finished. |
#'    | stadium | character | Name of the arena the game was played in. |
#'    | number_of_viewers | integer | Spectators at the game (attendance). |
#'    | referees_head_referee1_id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
#'    | referees_head_referee1_name | character | Official's full name. |
#'    | referees_head_referee2_id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
#'    | referees_head_referee2_name | character | Official's full name. |
#'    | referees_line_referee1_id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
#'    | referees_line_referee1_name | character | Official's full name. |
#'    | referees_line_referee2_id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
#'    | referees_line_referee2_name | character | Official's full name. |
#'    | team_info_home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_info_home_name | character | Team display name. |
#'    | team_info_home_shortcut | character | Three-letter team code. |
#'    | team_info_visitor_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_info_visitor_name | character | Team display name. |
#'    | team_info_visitor_shortcut | character | Three-letter team code. |
#'    | trainers_visitor_head_coach_id | integer | Coach person id, its own key space; not a player id; no other captured file joins to it. |
#'    | trainers_visitor_head_coach_name | character | Head coach full name. |
#'    | trainers_home_head_coach_id | integer | Coach person id, its own key space; not a player id; no other captured file joins to it. |
#'    | trainers_home_head_coach_name | character | Head coach full name. |
#'    | team_bilance_home_win | integer | Wins of that team in the head-to-head record (unverified). |
#'    | team_bilance_visitor_win | integer | Wins of that team in the head-to-head record (unverified). |
#'    | results_extra_time | logical | True when the game went to overtime. |
#'    | results_shooting | logical | True when the game was decided by a shootout. |
#'    | results_contumacy | logical | True when the result was awarded by forfeit. |
#'    | results_score_final_score_home | integer | Home team goals in that span. |
#'    | results_score_final_score_guest | integer | Visiting team goals in that span. |
#'    | results_score_first_period_score_home | integer | Home team goals in that span. |
#'    | results_score_first_period_score_guest | integer | Visiting team goals in that span. |
#'    | results_score_second_period_score_home | integer | Home team goals in that span. |
#'    | results_score_second_period_score_guest | integer | Visiting team goals in that span. |
#'    | results_score_third_period_score_home | integer | Home team goals in that span. |
#'    | results_score_third_period_score_guest | integer | Visiting team goals in that span. |
#'    | best_players_home_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | best_players_home_name | character | Player full name. |
#'    | best_players_visitor_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | best_players_visitor_name | character | Player full name. |
#'    | overtime_count | integer | Number of overtime periods; null in the samples. |
#'    | last_event_time | integer | Game second of the latest event (3600 = end of regulation). |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [DEL](https://www.penny-del.org/) (the league site whose widgets read this public eSports bucket)
#' @keywords DEL eSports
#' @family DEL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(del_game_info(match_id = 4444))
#' }
del_game_info <- function(match_id, return_parsed = TRUE) {
  .esports_game_info("del", match_id = match_id, return_parsed = return_parsed)
}

#' @title **DEL Pbp**
#' @description Play-by-play of one match: goals, penalties, goalie changes and timeouts, one row per event.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/{match_id}/period-events.json` of the DEL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/4444/period-events.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in del_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | event_index | integer | Row order of the event within the game (0-based), across periods. |
#'    | period | character | Period key as the body sends it: 1, 2, 3, OT (overtime) or SO (shootout). |
#'    | period_number | integer | Period number: 1-3, 4 = overtime, 5 = shootout. |
#'    | time | integer | Game second of the event from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | event_type | character | Event type: goal, penalty, goalkeeperChange, timeout, periodStart, periodEnd, videoReview, ... |
#'    | event_id | integer | Event id within its event type. |
#'    | team_side | character | Team of the row: home or visitor. |
#'    | goalie_player_id | integer | Player id (playerId key space) of the goalie entering the game (goalkeeperChange). |
#'    | goalie_jersey | integer | Jersey number of the goalie entering the game (goalkeeperChange). |
#'    | goalie_name | character | Full name of the goalie entering the game (goalkeeperChange). |
#'    | current_score | character | Score after the goal, as `home:visitor` text, e.g. `1:0`. |
#'    | real_time | character | Wall-clock local time, `YYYY-MM-DD HH:MM:SS`. |
#'    | scoreboard_time | integer | Seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | balance | character | Goal strength: `EQ` even, `PP1` / `PP2` power play, `SH1` / `SH2` shorthanded. |
#'    | extra_attacker | logical | True when the scoring team had an extra attacker on (goalie pulled). |
#'    | empty_net | logical | True when the goal went into an empty net. |
#'    | shooting | logical | True for a goal scored on a penalty shot. |
#'    | technical | logical | True for a technical penalty (unverified). |
#'    | scorer_player_id | integer | Player id (playerId key space) of the goal scorer. |
#'    | scorer_jersey | integer | Jersey number of the goal scorer. |
#'    | scorer_name | character | Full name of the goal scorer. |
#'    | assist1_player_id | integer | Player id (playerId key space) of the first assist. |
#'    | assist1_jersey | integer | Jersey number of the first assist. |
#'    | assist1_name | character | Full name of the first assist. |
#'    | assist2_player_id | integer | Player id (playerId key space) of the second assist. |
#'    | assist2_jersey | integer | Jersey number of the second assist. |
#'    | assist2_name | character | Full name of the second assist. |
#'    | plus_player_ids | character | Comma-separated player ids (playerId key space) of the scoring team's players on ice. |
#'    | minus_player_ids | character | Comma-separated player ids (playerId key space) of the conceding team's players on ice. |
#'    | create_time | integer | Game second at which the event was entered (unverified). |
#'    | penalized_player_id | integer | Player id (playerId key space) of the penalized player. |
#'    | penalized_jersey | integer | Jersey number of the penalized player. |
#'    | penalized_name | character | Full name of the penalized player. |
#'    | penalty_from_real_time | character | Penalty start, local wall clock YYYY-MM-DD HH:MM:SS. |
#'    | penalty_from_scoreboard_time | integer | Penalty start, game second. |
#'    | penalty_to_real_time | character | Penalty end, local wall clock YYYY-MM-DD HH:MM:SS. |
#'    | penalty_to_scoreboard_time | integer | Penalty end, game second. |
#'    | duration | integer | Penalty length in seconds (120 = minor). |
#'    | disciplined_functionary | logical | True when the penalty was assessed to a team official. |
#'    | disciplined_bm | logical | True when the penalty was a bench minor. |
#'    | codename | character | Penalty code, e.g. `HOOK`, `TRIP`, `ROUGH`, `BOARD`, `DELAY`. |
#'    | replaced_player_id | integer | Player id (playerId key space) of the player serving the penalty in the penalized player's place. |
#'    | replaced_jersey | integer | Jersey number of the player serving the penalty in the penalized player's place. |
#'    | replaced_name | character | Full name of the player serving the penalty in the penalized player's place. |
#'    | outgoing_goalie_player_id | integer | Player id (playerId key space) of the goalie leaving the game (goalkeeperChange). |
#'    | outgoing_goalie_jersey | integer | Jersey number of the goalie leaving the game (goalkeeperChange). |
#'    | outgoing_goalie_name | character | Full name of the goalie leaving the game (goalkeeperChange). |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | goal_review | character | Video review subject, e.g. a goal (EIHL `videoReview` events). |
#'    | on_ice_decision | character | Call on the ice before the review. |
#'    | reason_for_review | character | Why the play was reviewed (free text). |
#'    | decision_after_review | character | Final call after the review. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [DEL](https://www.penny-del.org/) (the league site whose widgets read this public eSports bucket)
#' @keywords DEL eSports
#' @family DEL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(del_pbp(match_id = 4444))
#' }
del_pbp <- function(match_id, return_parsed = TRUE) {
  .esports_pbp("del", match_id = match_id, return_parsed = return_parsed)
}

#' @title **DEL Roster**
#' @description Game roster of one match by line slot, one row per dressed player.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/{match_id}/roster.json` of the DEL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/4444/roster.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in del_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | team_side | character | Team of the row: home or visitor. |
#'    | slot | character | Line slot, three digits <group><line><position>: group 1 = goalies (101, 102), 2 = defence pairs, 3 = forward lines. |
#'    | player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | match_player_id | integer | Match-player id (one row per player per game; a different key space from the player id); joins to `matchPlayerId` in the same game's roster and events only. |
#'    | name | character | Player first name. |
#'    | surname | character | Player last name. |
#'    | jersey | integer | Jersey number. |
#'    | position | character | Position code: `GK` goalie, `DE` defence, `FW` / `FO` forward (FO meaning unverified). |
#'    | captain | logical | True for the team captain. |
#'    | starting_six | logical | True for the six players who started the game. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [DEL](https://www.penny-del.org/) (the league site whose widgets read this public eSports bucket)
#' @keywords DEL eSports
#' @family DEL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(del_roster(match_id = 4444))
#' }
del_roster <- function(match_id, return_parsed = TRUE) {
  .esports_roster("del", match_id = match_id, return_parsed = return_parsed)
}

#' @title **DEL Shifts**
#' @description Every shift of one match, one row per player shift.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/{match_id}/shiftsSC.json` of the DEL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/4444/shiftsSC.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in del_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | shift_id | integer | Shift id within the shift files. |
#'    | start_time | integer | Shift start, game second. |
#'    | start_real_time | character | Shift start, local wall clock. |
#'    | end_time | integer | Shift end, game second. |
#'    | end_real_time | character | Shift end, local wall clock. |
#'    | player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | player_name | character | Player full name. |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_name | character | Team display name. |
#'    | duration | integer | Shift length in seconds (end_time - start_time). |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [DEL](https://www.penny-del.org/) (the league site whose widgets read this public eSports bucket)
#' @keywords DEL eSports
#' @family DEL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(del_shifts(match_id = 4444))
#' }
del_shifts <- function(match_id, return_parsed = TRUE) {
  .esports_shifts("del", match_id = match_id, return_parsed = return_parsed)
}

#' @title **DEL Player Box**
#' @description Player boxscore of one team in one match, one row per player.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/{match_id}/team-stats/{team_id}.json` of the DEL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/4444/team-stats/12.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in del_schedule). Not the league website's game id.
#' @param team_id Team id (per-bucket team key space: home_id / guest_id in del_schedule).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (playerId key space, per bucket, stable across games and seasons). |
#'    | holema_id | integer | Unobserved (null in every sample); an external registry id by its name, key space unknown, joins to nothing captured. |
#'    | name | character | Player full name. |
#'    | firstname | character | First name of the player in this row. |
#'    | surname | character | Player last name. |
#'    | date_of_birth | character | Birth date `YYYY-MM-DD`. |
#'    | active | logical | True when the player is active on the team. |
#'    | position | character | Position code: `GK` goalie, `DE` defence, `FW` / `FO` forward (FO meaning unverified). |
#'    | stick | character | Handedness: `left` or `right`; null when unknown. |
#'    | weight | integer | Weight in kg; `0` placeholder when unknown. |
#'    | height | integer | Height in cm; `0` placeholder when unknown. |
#'    | jersey | integer | Jersey number. |
#'    | birth_country | character | Player's country of birth (name). |
#'    | nationality | character | Nationality, country name. |
#'    | nationality_short | character | Nationality, three-letter code (`CAN`, `GER`; `UK` for Britain). |
#'    | team_shortcut | character | Three-letter team code. |
#'    | games | integer | Games played. |
#'    | goals | integer | goals in total (goals_home + goals_away: the body splits it by home and away games). |
#'    | goals_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | goals_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists | integer | assists in total (assists_home + assists_away: the body splits it by home and away games). |
#'    | assists_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points | integer | points in total (points_home + points_away: the body splits it by home and away games). |
#'    | points_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | penalty_minutes | integer | Penalty minutes assessed to the player (this game, or to date in the season / league-part files). |
#'    | positive | integer | Plus: on ice for goals for at even strength or shorthanded. |
#'    | negative | integer | Minus: on ice for goals against at even strength or on the power play. |
#'    | points_per_game | numeric | Average goals plus assists per game played. |
#'    | shot_efficiency | numeric | Shooting percentage, goals / shots on goal x 100. |
#'    | penalty_minutes_per_game | numeric | Average penalty minutes per game played. |
#'    | pp_goals | integer | Power-play goals. |
#'    | sh_goals | integer | Shorthanded goals. |
#'    | gw_goals | integer | Game-winning goals. |
#'    | shots_attempts | integer | Shot attempts (on goal + missed + blocked). |
#'    | shots_on_goal | integer | shots_on_goal in total (shots_on_goal_home + shots_on_goal_away: the body splits it by home and away games). |
#'    | shots_on_goal_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_on_goal_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_missed | integer | This player's shot attempts that missed the net. |
#'    | shots_blocked | integer | This player's shot attempts that were blocked. |
#'    | blocked_shots_by_player | integer | Opponent shots this player blocked. |
#'    | faceoffs_win | integer | Face-offs the player won (this game, or to date in the season / league-part files). |
#'    | faceoffs_losses | integer | Face-offs the player lost (this game, or to date in the season / league-part files). |
#'    | faceoffs_count | integer | Face-offs the player took (this game, or to date in the season / league-part files). |
#'    | time_on_ice | integer | Time on ice, whole seconds (`0` when not tracked). |
#'    | time_on_ice_pp | integer | Time on ice on the power play (`PP`) or shorthanded (`SH`), whole seconds. |
#'    | time_on_ice_sh | integer | Time on ice on the power play (`PP`) or shorthanded (`SH`), whole seconds. |
#'    | time_on_ice_per_game | numeric | Time on ice per game, seconds. |
#'    | shifts | integer | Shifts the player took (this game, or to date in the season / league-part files). |
#'    | shifts_per_game | numeric | Average shifts per game played. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [DEL](https://www.penny-del.org/) (the league site whose widgets read this public eSports bucket)
#' @keywords DEL eSports
#' @family DEL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(del_player_box(match_id = 4444, team_id = 12))
#' }
del_player_box <- function(match_id, team_id, return_parsed = TRUE) {
  .esports_player_box("del", match_id = match_id, team_id = team_id, return_parsed = return_parsed)
}

#' @title **DEL Goalie Box**
#' @description Goalie lines of one match, one row per goalie who played.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/{match_id}/top-goalies.json` of the DEL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/4444/top-goalies.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in del_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | name | character | Goalie first name. |
#'    | surname | character | Goalie last name. |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_name | character | Team display name. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | jersey | integer | Jersey number. |
#'    | matches | integer | Games played (1 in a per-game file). |
#'    | won_matches | integer | Games the goalie won. |
#'    | lost_matches | integer | Games the goalie lost. |
#'    | saves | integer | Shots on goal the goalie stopped. |
#'    | goals_against | integer | Goals the goalie allowed. |
#'    | saves_percent | numeric | Save percentage x 100 (e.g. `85.71`). |
#'    | average | numeric | Goals-against average per 60 minutes. |
#'    | zeros | integer | Shutouts by the goalie (`zeros`: games with no goal allowed). |
#'    | time_on_ice | integer | Time in goal, seconds. |
#'    | seconds_per_game | numeric | Time in goal per game, seconds. |
#'    | minutes_in_play | numeric | Share of the team's game time in goal, percent (100.0 = the whole game). |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [DEL](https://www.penny-del.org/) (the league site whose widgets read this public eSports bucket)
#' @keywords DEL eSports
#' @family DEL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(del_goalie_box(match_id = 4444))
#' }
del_goalie_box <- function(match_id, return_parsed = TRUE) {
  .esports_goalie_box("del", match_id = match_id, return_parsed = return_parsed)
}

#' @title **DEL Standings**
#' @description Standings of one competition season, one row per team.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/tables/{season}/{league_id}.json` of the DEL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/tables/2026/1.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param league_id Competition id. 1 = DEL regular season, 3 = DEL playoffs (also 2 = Test-Liga pre-season, 4 = MagentaSport Cup 2020/21, 5 = NHL Global Series 2022/23).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | league_part_id | integer | League part id (league-part key space, per bucket: one stage of a competition, e.g. ICEHL 56 = 2026/27 regular season); joins to `league_part_id` in league-matches and the `{league_part_id}` path param. |
#'    | league_part_name | character | League part name, e.g. `DEL 2026/27`. |
#'    | state | character | Standings state flag (e.g. live or final; values unverified). |
#'    | datetime | character | Standings computed at, local time, `YYYY-MM-DD HH:MM:SS`. |
#'    | team_id | integer | Team id (per-bucket team key space); joins to home_id / guest_id in the schedule. |
#'    | part_team_id | integer | Team-in-league-part id (its own key space: one team in one league part); no other captured file carries it. |
#'    | name | character | Team display name. |
#'    | shortcut | character | Three-letter team code. |
#'    | position | integer | Table position, 1 = first. |
#'    | points | integer | Table points. |
#'    | games | integer | Games played. |
#'    | wins | integer | Wins in regulation (DEL, EIHL, Denmark) or all wins (ICEHL, see `winsReg`); definition per bucket unverified. |
#'    | wins_ot | integer | Wins after overtime or shootout. |
#'    | ties | integer | Ties (0 under current rules). |
#'    | losses_ot | integer | Losses after overtime or shootout. |
#'    | losses | integer | Losses in regulation time. |
#'    | score1 | integer | Goals the team scored (goals for). |
#'    | score2 | integer | Goals against. |
#'    | only_so_wins | integer | Wins in a shootout. |
#'    | only_so_losts | integer | Losses in a shootout. |
#'    | only_ot_wins | integer | Wins in overtime. |
#'    | only_ot_losts | integer | Losses in overtime. |
#'    | bonus | integer | Bonus or deduction points (DEL). |
#'    | ppg | numeric | Points per game. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [DEL](https://www.penny-del.org/) (the league site whose widgets read this public eSports bucket)
#' @keywords DEL eSports
#' @family DEL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(del_standings(season = 2027, league_id = 1))
#' }
del_standings <- function(season, league_id = 1L, return_parsed = TRUE) {
  .esports_standings("del", season = season, league_id = league_id, return_parsed = return_parsed)
}

#' @title **DEL Shots**
#' @description Shots of one match with rink coordinates and result, one row per shot.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/visualization/shots/{match_id}.json` of the DEL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/visualization/shots/4444.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in del_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | match_id | integer | Match id (per-bucket match key space); joins to the `{match_id}` path param of the game wrappers. |
#'    | date | character | Game date `YYYY-MM-DD`. |
#'    | home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | home_name | character | Team display name. |
#'    | home_shortcut | character | Three-letter team code. |
#'    | visitor_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | visitor_name | character | Team display name. |
#'    | visitor_shortcut | character | Three-letter team code. |
#'    | shot_id | integer | Shot id within the shot files. |
#'    | player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | jersey | integer | Jersey number. |
#'    | first_name | character | First name of the shooter. |
#'    | last_name | character | Last name of the shooter. |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | match_shot_resutl_id | integer | Shot result code (the provider's typo kept): `4` = goal; `1` / `2` / `3` probably on goal / missed / blocked (unverified). A code, not a joinable id. |
#'    | coordinate_x | numeric | Shot x along the rink LENGTH, centre ice = 0, about -100..100 (units unverified, plausibly percent of half-length). Normalised to the attacking direction: each team keeps one sign for the whole game, the home team positive in the three sample games. DEL sends two-decimal floats, ICEHL and EIHL whole numbers (`-71.0`). |
#'    | coordinate_y | numeric | Shot y across the rink WIDTH, centre line = 0, about -100..100 (units and which board is positive unverified). DEL two-decimal floats, ICEHL and EIHL whole numbers. |
#'    | real_date | character | Wall-clock local time, `YYYY-MM-DD HH:MM:SS`. |
#'    | polygon | character | Rink area of the shot, `{home/away}_{top/center/bottom/defense}_polygon` (DEL). |
#'    | time | integer | Seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | shot_result_id | integer | Shot result code, the same value as match_shot_resutl_id: 4 = goal (verified); 1, 2 and 3 (on goal / missed / blocked in the widgets) are unverified. |
#'    | is_goal | logical | True when the shot result code is 4 (goal). |
#'    | team_side | character | Team of the row: home or visitor. |
#'    | xg | numeric | Expected-goals value of the shot (ICEHL and EIHL only; null elsewhere). |
#'    | xg_type | character | Shot-danger class from the xG model, e.g. LD / MD / HD (ICEHL and EIHL only; null elsewhere). |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [DEL](https://www.penny-del.org/) (the league site whose widgets read this public eSports bucket)
#' @keywords DEL eSports
#' @family DEL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(del_shots(match_id = 4444))
#' }
del_shots <- function(match_id, return_parsed = TRUE) {
  .esports_shots("del", match_id = match_id, return_parsed = return_parsed)
}

#' @title **ICE Hockey League Schedule**
#' @description Schedule and results of one competition season, one row per match, played or not.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/league-matches/{season}/{league_id}.json` of the ICE Hockey League eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/league-matches/2026/1.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param league_id Competition id. 1 = ICE Hockey League, regular season and playoffs in one file (split them by league_part_name); 2 = pre-season (2025/26 on).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | match_id | integer | Match id (per-bucket match key space); joins to the `{match_id}` path param of the game wrappers. |
#'    | start_date | character | Scheduled face-off, local `YYYY-MM-DD HH:MM:SS`; DEL playoff placeholders read `0000-00-00 ...`. |
#'    | real_start_date | character | Actual start, local `YYYY-MM-DD HH:MM:SS`; `0000-00-00 00:00:00` placeholder before the game. |
#'    | real_end_date | character | Actual end, local `YYYY-MM-DD HH:MM:SS`. |
#'    | league_id | integer | League id (per-bucket competition key space); joins to the `{league_id}` path param. |
#'    | league | character | League display name, e.g. `DEL`, `EIHL`, `Metal Ligaen`. |
#'    | league_part_id | integer | League part id (league-part key space, per bucket: one stage of a competition, e.g. ICEHL 56 = 2026/27 regular season); joins to `league_part_id` in league-matches and the `{league_part_id}` path param. |
#'    | league_part_name | character | League part name, e.g. `ICEHL 2026/27` or `... - Playoffs`; splits regular season from playoffs where one league id holds both. |
#'    | season | integer | Season by its END year (2027 = 2026/27); the body's START-year key plus one. |
#'    | round | character | Round number as a string (`"1"`, `"249"`); DEL values carry a leading U+FEFF byte-order mark, empty string in Denmark. |
#'    | highlights | character | Highlights video URL; empty string or null when none. |
#'    | match_number | integer | Match number within the league part (ICEHL). |
#'    | bets_home_win | numeric | Decimal odds for that outcome; null when not offered. |
#'    | bets_draw | numeric | Decimal odds for that outcome; null when not offered. |
#'    | bets_visitor_win | numeric | Decimal odds for that outcome; null when not offered. |
#'    | group | character | Group name in group stages; empty string or null otherwise. |
#'    | home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | home_name | character | Team display name. |
#'    | home_shortcut | character | Three-letter team code. |
#'    | guest_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | guest_name | character | Team display name. |
#'    | guest_shortcut | character | Three-letter team code. |
#'    | status | character | Match status: `BEFORE_MATCH`, `LIVE`, `AFTER_MATCH`. |
#'    | overtime_count | integer | Number of overtime periods played; null in the samples. |
#'    | extra_time | logical | True when the game went to overtime. |
#'    | shooting | logical | True when the game was decided by a shootout. |
#'    | contumacy | logical | True when the result was awarded by forfeit. |
#'    | final_score_home | integer | Home team goals in that span. |
#'    | final_score_guest | integer | Visiting team goals in that span. |
#'    | first_period_score_home | integer | Home team goals in that span. |
#'    | first_period_score_guest | integer | Visiting team goals in that span. |
#'    | second_period_score_home | integer | Home team goals in that span. |
#'    | second_period_score_guest | integer | Visiting team goals in that span. |
#'    | third_period_score_home | integer | Home team goals in that span. |
#'    | third_period_score_guest | integer | Visiting team goals in that span. |
#'    | actual_time_name | character | Display label of the game state in the bucket's language, e.g. `end`, `Ende`, `before game`. |
#'    | actual_time_alias | character | Short game-state code: `K` = finished (Czech `konec`), `0` = not started. |
#'    | last_event_time | integer | Game second of the latest event, of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | arena | character | Arena name; empty string or null where not set. |
#'    | advanced_statistics | logical | True when the game has advanced stats files (shots, boxscore). |
#'    | tickets_url | character | URL of the ticket shop for the match. |
#'    | television_name | character | Broadcaster name. |
#'    | television_url | character | Broadcaster URL. |
#'    | television_logo | character | Broadcaster logo image URL. |
#'    | television2 | character | Second broadcaster, same shape as `television`; null in the samples. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [ICE Hockey League](https://www.ice.hockey/) (the league site whose widgets read this public eSports bucket)
#' @keywords ICE Hockey League eSports
#' @family ICE Hockey League eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(icehl_schedule(season = 2027, league_id = 1))
#' }
icehl_schedule <- function(season, league_id = 1L, return_parsed = TRUE) {
  .esports_schedule("icehl", season = season, league_id = league_id, return_parsed = return_parsed)
}

#' @title **ICE Hockey League League Part Player Stats**
#' @description Every skater's stats in one league part (a competition stage), one row per player.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/league-part-players-stats/{league_part_id}.json` of the ICE Hockey League eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/league-part-players-stats/56.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param league_part_id League part id, one stage of a competition (league_part_id in icehl_schedule).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (playerId key space, per bucket, stable across seasons). |
#'    | name | character | Player full name. |
#'    | firstname | character | First name of the player in this row. |
#'    | surname | character | Player last name. |
#'    | birthdate | character | Birth date `YYYY-MM-DD`. |
#'    | position | character | Position code: `GK` goalie, `DE` defence, `FW` / `FO` forward (FO meaning unverified). |
#'    | stick | character | Handedness: `left` or `right`; null when unknown. |
#'    | weight | integer | Weight in kg; `0` placeholder when unknown. |
#'    | height | integer | Height in cm; `0` placeholder when unknown. |
#'    | jersey | integer | Jersey number. |
#'    | birth_country | character | Player's country of birth (name). |
#'    | nationality | character | Nationality, country name. |
#'    | nationality_short | character | Nationality, three-letter code (`CAN`, `GER`; `UK` for Britain). |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | games | integer | Games played. |
#'    | goals | integer | goals in total (goals_home + goals_away: the body splits it by home and away games). |
#'    | goals_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | goals_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists | integer | assists in total (assists_home + assists_away: the body splits it by home and away games). |
#'    | assists_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points | integer | points in total (points_home + points_away: the body splits it by home and away games). |
#'    | points_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | penalty_minutes | integer | Penalty minutes assessed to the player (this game, or to date in the season / league-part files). |
#'    | positive | integer | Plus: on ice for goals for at even strength or shorthanded. |
#'    | negative | integer | Minus: on ice for goals against at even strength or on the power play. |
#'    | points_per_game | numeric | Average goals plus assists per game played. |
#'    | shot_efficiency | numeric | Shooting percentage, goals / shots on goal x 100. |
#'    | penalty_minutes_per_game | numeric | Average penalty minutes per game played. |
#'    | pp_goals | integer | Power-play goals. |
#'    | sh_goals | integer | Shorthanded goals. |
#'    | gw_goals | integer | Game-winning goals. |
#'    | shots_attempts | integer | Shot attempts (on goal + missed + blocked). |
#'    | shots_on_goal | integer | shots_on_goal in total (shots_on_goal_home + shots_on_goal_away: the body splits it by home and away games). |
#'    | shots_on_goal_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_on_goal_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_missed | integer | This player's shot attempts that missed the net. |
#'    | shots_blocked | integer | This player's shot attempts that were blocked. |
#'    | blocked_shots_by_player | integer | Opponent shots this player blocked. |
#'    | faceoffs_win | integer | Face-offs the player won (this game, or to date in the season / league-part files). |
#'    | faceoffs_losses | integer | Face-offs the player lost (this game, or to date in the season / league-part files). |
#'    | faceoffs_count | integer | Face-offs the player took (this game, or to date in the season / league-part files). |
#'    | timeonice | numeric | Time on ice, seconds with decimals (tracking-based; differs from `timeOnIce`). |
#'    | timeonice_eq | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | timeonice_pp | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | timeonice_sh | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | hits | integer | Body checks delivered. |
#'    | visible_games | logical | Widget flag: show the milestone for that stat. |
#'    | visible_goals | logical | Widget flag: show the milestone for that stat. |
#'    | visible_assists | logical | Widget flag: show the milestone for that stat. |
#'    | visible_points | logical | Widget flag: show the milestone for that stat. |
#'    | stars_games | integer | Widget star marker for that stat (unverified). |
#'    | stars_goals | integer | Widget star marker for that stat (unverified). |
#'    | stars_assists | integer | Widget star marker for that stat (unverified). |
#'    | stars_points | integer | Widget star marker for that stat (unverified). |
#'    | milestone_games | numeric | Widget value for the next milestone of that stat (unverified). |
#'    | milestone_goals | numeric | Widget value for the next milestone of that stat (unverified). |
#'    | milestone_assists | numeric | Widget value for the next milestone of that stat (unverified). |
#'    | milestone_points | numeric | Widget value for the next milestone of that stat (unverified). |
#'    | toi_gp | numeric | Time on ice per game played, seconds with decimals. |
#'    | last_goal | character | Opaque date-like code of the player's latest goal, e.g. `2026-0-09-02640`; format unverified. |
#'    | rank | integer | Rank of the player in the file's default sort (points). |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [ICE Hockey League](https://www.ice.hockey/) (the league site whose widgets read this public eSports bucket)
#' @keywords ICE Hockey League eSports
#' @family ICE Hockey League eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(icehl_league_part_player_stats(league_part_id = 56))
#' }
icehl_league_part_player_stats <- function(league_part_id, return_parsed = TRUE) {
  .esports_league_part_player_stats("icehl", league_part_id = league_part_id, return_parsed = return_parsed)
}

#' @title **ICE Hockey League Team Player Stats**
#' @description Season player stats of one team in one competition, one row per player.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/league-team-stats/{season}/{league_id}/{team_id}.json` of the ICE Hockey League eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/league-team-stats/2026/1/4.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param team_id Team id (per-bucket team key space: home_id / guest_id in icehl_schedule).
#' @param league_id Competition id. 1 = ICE Hockey League, regular season and playoffs in one file (split them by league_part_name); 2 = pre-season (2025/26 on).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (playerId key space, per bucket, stable across games and seasons). |
#'    | name | character | Player full name. |
#'    | firstname | character | First name of the player in this row. |
#'    | surname | character | Player last name. |
#'    | photo | character | Player photo URL. |
#'    | birthdate | character | Birth date `YYYY-MM-DD`. |
#'    | active | logical | True when the player is active on the team. |
#'    | position | character | Position code: `GK` goalie, `DE` defence, `FW` / `FO` forward (FO meaning unverified). |
#'    | stick | character | Handedness: `left` or `right`; null when unknown. |
#'    | weight | integer | Weight in kg; `0` placeholder when unknown. |
#'    | height | integer | Height in cm; `0` placeholder when unknown. |
#'    | jersey | integer | Jersey number. |
#'    | birth_country | character | Player's country of birth (name). |
#'    | nationality | character | Nationality, country name. |
#'    | nationality_short | character | Nationality, three-letter code (`CAN`, `GER`; `UK` for Britain). |
#'    | on_roster | integer | On-roster flag sent as an integer, `1` = yes. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | games | integer | Games played. |
#'    | goals | integer | goals in total (goals_home + goals_away: the body splits it by home and away games). |
#'    | goals_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | goals_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists | integer | assists in total (assists_home + assists_away: the body splits it by home and away games). |
#'    | assists_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points | integer | points in total (points_home + points_away: the body splits it by home and away games). |
#'    | points_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | penalty_minutes | integer | Penalty minutes assessed to the player (this game, or to date in the season / league-part files). |
#'    | positive | integer | Plus: on ice for goals for at even strength or shorthanded. |
#'    | negative | integer | Minus: on ice for goals against at even strength or on the power play. |
#'    | points_per_game | numeric | Average goals plus assists per game played. |
#'    | shot_efficiency | numeric | Shooting percentage, goals / shots on goal x 100. |
#'    | penalty_minutes_per_game | numeric | Average penalty minutes per game played. |
#'    | pp_goals | integer | Power-play goals. |
#'    | sh_goals | integer | Shorthanded goals. |
#'    | gw_goals | integer | Game-winning goals. |
#'    | shots_attempts | integer | Shot attempts (on goal + missed + blocked). |
#'    | shots_on_goal | integer | shots_on_goal in total (shots_on_goal_home + shots_on_goal_away: the body splits it by home and away games). |
#'    | shots_on_goal_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_on_goal_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_missed | integer | This player's shot attempts that missed the net. |
#'    | shots_blocked | integer | This player's shot attempts that were blocked. |
#'    | blocked_shots_by_player | integer | Opponent shots this player blocked. |
#'    | faceoffs_win | integer | Face-offs the player won (this game, or to date in the season / league-part files). |
#'    | faceoffs_losses | integer | Face-offs the player lost (this game, or to date in the season / league-part files). |
#'    | faceoffs_count | integer | Face-offs the player took (this game, or to date in the season / league-part files). |
#'    | timeonice | numeric | Time on ice, seconds with decimals (tracking-based; differs from `timeOnIce`). |
#'    | timeonice_eq | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | timeonice_pp | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | timeonice_sh | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | hits | integer | Body checks delivered. |
#'    | xg | numeric | Expected goals of the player's shots. |
#'    | rank | integer | Rank of the player in the file's default sort (points). |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [ICE Hockey League](https://www.ice.hockey/) (the league site whose widgets read this public eSports bucket)
#' @keywords ICE Hockey League eSports
#' @family ICE Hockey League eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(icehl_team_player_stats(season = 2027, team_id = 4, league_id = 1))
#' }
icehl_team_player_stats <- function(season, team_id, league_id = 1L, return_parsed = TRUE) {
  .esports_team_player_stats("icehl", season = season, team_id = team_id, league_id = league_id, return_parsed = return_parsed)
}

#' @title **ICE Hockey League Top Scorers**
#' @description Skater scoring leaderboard of one competition season.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/league-top-scorers/{season}/{league_id}.json` of the ICE Hockey League eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/league-top-scorers/2026/1.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param league_id Competition id. 1 = ICE Hockey League, regular season and playoffs in one file (split them by league_part_name); 2 = pre-season (2025/26 on).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | name | character | Player first name. |
#'    | surname | character | Player last name. |
#'    | photo | character | Player photo URL; null when none. |
#'    | birthdate | character | Birth date `YYYY-MM-DD`. |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_name | character | Team display name. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | goals | integer | Goals the player scored in the league-season. |
#'    | assists | integer | Assists credited to the player in the league-season. |
#'    | points | integer | Points, goals + assists. |
#'    | shots | integer | Shots on goal. |
#'    | penalty_minutes | integer | Penalty minutes assessed to the player in the league-season. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [ICE Hockey League](https://www.ice.hockey/) (the league site whose widgets read this public eSports bucket)
#' @keywords ICE Hockey League eSports
#' @family ICE Hockey League eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(icehl_top_scorers(season = 2027, league_id = 1))
#' }
icehl_top_scorers <- function(season, league_id = 1L, return_parsed = TRUE) {
  .esports_top_scorers("icehl", season = season, league_id = league_id, return_parsed = return_parsed)
}

#' @title **ICE Hockey League Faceoffs**
#' @description Every faceoff of one match: time, dot, winner and loser.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/matches/{match_id}/faceoffs.json` of the ICE Hockey League eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/matches/8161/faceoffs.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in icehl_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | id | integer | Face-off id (face-off key space); no other captured file joins to it. |
#'    | time | integer | Seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | realtime | character | Wall-clock local time, `YYYY-MM-DD HH:MM:SS`. |
#'    | position | character | Face-off dot: `BULY_CENTRAL` or `BULY_{HOME/AWAY}_{DEFENSE/BLUE}_{LEFT/RIGHT}`. |
#'    | position_shortcut | character | Short dot code, e.g. `C`, `HDL` (home defence left), `HDR`. |
#'    | winner_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | winner_name | character | Player full name. |
#'    | loser_id | integer | Player id (playerId key space) of the faceoff loser (DEL spells the key losser, ICEHL looser). |
#'    | loser_name | character | Full name of the faceoff loser. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [ICE Hockey League](https://www.ice.hockey/) (the league site whose widgets read this public eSports bucket)
#' @keywords ICE Hockey League eSports
#' @family ICE Hockey League eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(icehl_faceoffs(match_id = 8161))
#' }
icehl_faceoffs <- function(match_id, return_parsed = TRUE) {
  .esports_faceoffs("icehl", match_id = match_id, return_parsed = return_parsed)
}

#' @title **ICE Hockey League Game Info**
#' @description Game info of one match: state, arena, attendance, officials, coaches, period scores.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/matches/{match_id}/game-header.json` of the ICE Hockey League eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/matches/8161/game-header.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in icehl_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | actual_time_name | character | Display label of the game state, e.g. `end` / `Ende`. |
#'    | actual_time_alias | character | Short game-state code: `K` = finished. |
#'    | status | character | Match status: `BEFORE_MATCH`, `LIVE`, `AFTER_MATCH`. |
#'    | stadium | character | Name of the arena the game was played in. |
#'    | number_of_viewers | integer | Spectators at the game (attendance). |
#'    | referees_head_referee1 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | referees_head_referee2 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | referees_line_referee1 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | referees_line_referee2 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | referees_head_referee1number | character | Official's sweater number; null when not recorded. |
#'    | referees_head_referee2number | character | Official's sweater number; null when not recorded. |
#'    | referees_line_referee1number | character | Official's sweater number; null when not recorded. |
#'    | referees_line_referee2number | character | Official's sweater number; null when not recorded. |
#'    | team_info_home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_info_home_name | character | Team display name. |
#'    | team_info_home_shortcut | character | Three-letter team code. |
#'    | team_info_visitor_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_info_visitor_name | character | Team display name. |
#'    | team_info_visitor_shortcut | character | Three-letter team code. |
#'    | trainers_visitor_head_coach | character | Head coach (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | trainers_home_head_coach | character | Head coach (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | team_bilance_home_win | integer | Wins of that team in the head-to-head record (unverified). |
#'    | team_bilance_visitor_win | integer | Wins of that team in the head-to-head record (unverified). |
#'    | results_extra_time | logical | True when the game went to overtime. |
#'    | results_shooting | logical | True when the game was decided by a shootout. |
#'    | results_contumacy | logical | True when the result was awarded by forfeit. |
#'    | results_score_final_score_home | integer | Home team goals in that span. |
#'    | results_score_final_score_guest | integer | Visiting team goals in that span. |
#'    | results_score_first_period_score_home | integer | Home team goals in that span. |
#'    | results_score_first_period_score_guest | integer | Visiting team goals in that span. |
#'    | results_score_second_period_score_home | integer | Home team goals in that span. |
#'    | results_score_second_period_score_guest | integer | Visiting team goals in that span. |
#'    | results_score_third_period_score_home | integer | Home team goals in that span. |
#'    | results_score_third_period_score_guest | integer | Visiting team goals in that span. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [ICE Hockey League](https://www.ice.hockey/) (the league site whose widgets read this public eSports bucket)
#' @keywords ICE Hockey League eSports
#' @family ICE Hockey League eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(icehl_game_info(match_id = 8161))
#' }
icehl_game_info <- function(match_id, return_parsed = TRUE) {
  .esports_game_info("icehl", match_id = match_id, return_parsed = return_parsed)
}

#' @title **ICE Hockey League Pbp**
#' @description Play-by-play of one match: goals, penalties, goalie changes and timeouts, one row per event.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/matches/{match_id}/period-events.json` of the ICE Hockey League eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/matches/8161/period-events.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in icehl_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | event_index | integer | Row order of the event within the game (0-based), across periods. |
#'    | period | character | Period key as the body sends it: 1, 2, 3, OT (overtime) or SO (shootout). |
#'    | period_number | integer | Period number: 1-3, 4 = overtime, 5 = shootout. |
#'    | time | integer | Game second of the event from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | event_type | character | Event type: goal, penalty, goalkeeperChange, timeout, periodStart, periodEnd, videoReview, ... |
#'    | event_id | integer | Event id within its event type. |
#'    | team_side | character | Team of the row: home or visitor. |
#'    | goalie_player_id | integer | Player id (playerId key space) of the goalie entering the game (goalkeeperChange). |
#'    | goalie_jersey | integer | Jersey number of the goalie entering the game (goalkeeperChange). |
#'    | goalie_name | character | Full name of the goalie entering the game (goalkeeperChange). |
#'    | current_score | character | Score after the goal, as `home:visitor` text, e.g. `1:0`. |
#'    | real_time | character | Wall-clock local time, `YYYY-MM-DD HH:MM:SS`. |
#'    | scoreboard_time | integer | Seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | balance | character | Goal strength: `EQ` even, `PP1` / `PP2` power play, `SH1` / `SH2` shorthanded. |
#'    | extra_attacker | logical | True when the scoring team had an extra attacker on (goalie pulled). |
#'    | empty_net | logical | True when the goal went into an empty net. |
#'    | shooting | logical | True for a goal scored on a penalty shot. |
#'    | technical | logical | True for a technical penalty (unverified). |
#'    | scorer_player_id | integer | Player id (playerId key space) of the goal scorer. |
#'    | scorer_jersey | integer | Jersey number of the goal scorer. |
#'    | scorer_name | character | Full name of the goal scorer. |
#'    | assist1_player_id | integer | Player id (playerId key space) of the first assist. |
#'    | assist1_jersey | integer | Jersey number of the first assist. |
#'    | assist1_name | character | Full name of the first assist. |
#'    | assist2_player_id | integer | Player id (playerId key space) of the second assist. |
#'    | assist2_jersey | integer | Jersey number of the second assist. |
#'    | assist2_name | character | Full name of the second assist. |
#'    | plus_player_ids | character | Comma-separated player ids (playerId key space) of the scoring team's players on ice. |
#'    | minus_player_ids | character | Comma-separated player ids (playerId key space) of the conceding team's players on ice. |
#'    | create_time | integer | Game second at which the event was entered (unverified). |
#'    | penalized_player_id | integer | Player id (playerId key space) of the penalized player. |
#'    | penalized_jersey | integer | Jersey number of the penalized player. |
#'    | penalized_name | character | Full name of the penalized player. |
#'    | penalty_from_real_time | character | Penalty start, local wall clock YYYY-MM-DD HH:MM:SS. |
#'    | penalty_from_scoreboard_time | integer | Penalty start, game second. |
#'    | penalty_to_real_time | character | Penalty end, local wall clock YYYY-MM-DD HH:MM:SS. |
#'    | penalty_to_scoreboard_time | integer | Penalty end, game second. |
#'    | duration | integer | Penalty length in seconds (120 = minor). |
#'    | disciplined_functionary | logical | True when the penalty was assessed to a team official. |
#'    | disciplined_bm | logical | True when the penalty was a bench minor. |
#'    | codename | character | Penalty code, e.g. `HOOK`, `TRIP`, `ROUGH`, `BOARD`, `DELAY`. |
#'    | outgoing_goalie_player_id | integer | Player id (playerId key space) of the goalie leaving the game (goalkeeperChange). |
#'    | outgoing_goalie_jersey | integer | Jersey number of the goalie leaving the game (goalkeeperChange). |
#'    | outgoing_goalie_name | character | Full name of the goalie leaving the game (goalkeeperChange). |
#'    | replaced_player_id | integer | Player id (playerId key space) of the player serving the penalty in the penalized player's place. |
#'    | replaced_jersey | integer | Jersey number of the player serving the penalty in the penalized player's place. |
#'    | replaced_name | character | Full name of the player serving the penalty in the penalized player's place. |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | goal_review | character | Video review subject, e.g. a goal (EIHL `videoReview` events). |
#'    | on_ice_decision | character | Call on the ice before the review. |
#'    | reason_for_review | character | Why the play was reviewed (free text). |
#'    | decision_after_review | character | Final call after the review. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [ICE Hockey League](https://www.ice.hockey/) (the league site whose widgets read this public eSports bucket)
#' @keywords ICE Hockey League eSports
#' @family ICE Hockey League eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(icehl_pbp(match_id = 8161))
#' }
icehl_pbp <- function(match_id, return_parsed = TRUE) {
  .esports_pbp("icehl", match_id = match_id, return_parsed = return_parsed)
}

#' @title **ICE Hockey League Player Box**
#' @description Player boxscore of one team in one match, one row per player.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/matches/{match_id}/team-stats/{team_id}.json` of the ICE Hockey League eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/matches/8161/team-stats/4.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in icehl_schedule). Not the league website's game id.
#' @param team_id Team id (per-bucket team key space: home_id / guest_id in icehl_schedule).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (playerId key space, per bucket, stable across games and seasons). |
#'    | name | character | Player full name. |
#'    | firstname | character | First name of the player in this row. |
#'    | surname | character | Player last name. |
#'    | photo | character | Player photo URL. |
#'    | birthdate | character | Birth date `YYYY-MM-DD`. |
#'    | active | logical | True when the player is active on the team. |
#'    | position | character | Position code: `GK` goalie, `DE` defence, `FW` / `FO` forward (FO meaning unverified). |
#'    | stick | character | Handedness: `left` or `right`; null when unknown. |
#'    | weight | integer | Weight in kg; `0` placeholder when unknown. |
#'    | height | integer | Height in cm; `0` placeholder when unknown. |
#'    | jersey | integer | Jersey number. |
#'    | birth_country | character | Player's country of birth (name). |
#'    | nationality | character | Nationality, country name. |
#'    | nationality_short | character | Nationality, three-letter code (`CAN`, `GER`; `UK` for Britain). |
#'    | on_roster | integer | On-roster flag sent as an integer, `1` = yes. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | games | integer | Games played. |
#'    | goals | integer | goals in total (goals_home + goals_away: the body splits it by home and away games). |
#'    | goals_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | goals_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists | integer | assists in total (assists_home + assists_away: the body splits it by home and away games). |
#'    | assists_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points | integer | points in total (points_home + points_away: the body splits it by home and away games). |
#'    | points_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | penalty_minutes | integer | Penalty minutes assessed to the player (this game, or to date in the season / league-part files). |
#'    | positive | integer | Plus: on ice for goals for at even strength or shorthanded. |
#'    | negative | integer | Minus: on ice for goals against at even strength or on the power play. |
#'    | points_per_game | numeric | Average goals plus assists per game played. |
#'    | shot_efficiency | numeric | Shooting percentage, goals / shots on goal x 100. |
#'    | penalty_minutes_per_game | numeric | Average penalty minutes per game played. |
#'    | pp_goals | integer | Power-play goals. |
#'    | sh_goals | integer | Shorthanded goals. |
#'    | gw_goals | integer | Game-winning goals. |
#'    | shots_attempts | integer | Shot attempts (on goal + missed + blocked). |
#'    | shots_on_goal | integer | shots_on_goal in total (shots_on_goal_home + shots_on_goal_away: the body splits it by home and away games). |
#'    | shots_on_goal_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_on_goal_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_missed | integer | This player's shot attempts that missed the net. |
#'    | shots_blocked | integer | This player's shot attempts that were blocked. |
#'    | blocked_shots_by_player | integer | Opponent shots this player blocked. |
#'    | faceoffs_win | integer | Face-offs the player won (this game, or to date in the season / league-part files). |
#'    | faceoffs_losses | integer | Face-offs the player lost (this game, or to date in the season / league-part files). |
#'    | faceoffs_count | integer | Face-offs the player took (this game, or to date in the season / league-part files). |
#'    | timeonice | numeric | Time on ice, seconds with decimals (tracking-based; differs from `timeOnIce`). |
#'    | timeonice_eq | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | timeonice_pp | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | timeonice_sh | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | hits | integer | Body checks delivered. |
#'    | xg | numeric | Expected goals of the player's shots. |
#'    | top5 | logical | True when the player ranks in the team's top five. |
#'    | rank | integer | Rank of the player in the file's default sort (points). |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [ICE Hockey League](https://www.ice.hockey/) (the league site whose widgets read this public eSports bucket)
#' @keywords ICE Hockey League eSports
#' @family ICE Hockey League eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(icehl_player_box(match_id = 8161, team_id = 4))
#' }
icehl_player_box <- function(match_id, team_id, return_parsed = TRUE) {
  .esports_player_box("icehl", match_id = match_id, team_id = team_id, return_parsed = return_parsed)
}

#' @title **ICE Hockey League Goalie Box**
#' @description Goalie lines of one match, one row per goalie who played.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/matches/{match_id}/top-goalies.json` of the ICE Hockey League eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/matches/8161/top-goalies.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in icehl_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | name | character | Goalie first name. |
#'    | surname | character | Goalie last name. |
#'    | photo | character | Goalie photo URL; null when none. |
#'    | birthdate | character | Birth date `YYYY-MM-DD`. |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_name | character | Team display name. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | team_type | character | Side: `home` or `visitor`; null in DEL and EIHL. |
#'    | jersey | integer | Jersey number. |
#'    | matches | integer | Games played (1 in a per-game file). |
#'    | won_matches | integer | Games the goalie won. |
#'    | lost_matches | integer | Games the goalie lost. |
#'    | saves | integer | Shots on goal the goalie stopped. |
#'    | goals_against | integer | Goals the goalie allowed. |
#'    | shots_against | integer | Shots on goal faced. |
#'    | saves_percent | numeric | Save percentage x 100 (e.g. `85.71`). |
#'    | average | numeric | Goals-against average per 60 minutes. |
#'    | zeros | integer | Shutouts by the goalie (`zeros`: games with no goal allowed). |
#'    | time_on_ice | integer | Time in goal, seconds. |
#'    | seconds_per_game | numeric | Time in goal per game, seconds. |
#'    | minutes_in_play | numeric | Share of the team's game time in goal, percent (100.0 = the whole game). |
#'    | visible_games | logical | Widget flag: show the milestone for that stat. |
#'    | visible_shutouts | logical | Widget flag: show the milestone for that stat. |
#'    | visible_wins | logical | Widget flag: show the milestone for that stat. |
#'    | stars_games | integer | Widget star marker for that stat (unverified). |
#'    | stars_shutouts | integer | Widget star marker for that stat (unverified). |
#'    | stars_wins | integer | Widget star marker for that stat (unverified). |
#'    | milestone_games | numeric | Widget value for the next milestone of that stat (unverified). |
#'    | milestone_shutouts | numeric | Widget value for the next milestone of that stat (unverified). |
#'    | milestone_wins | numeric | Widget value for the next milestone of that stat (unverified). |
#'    | xg | numeric | Expected goals of the goalie's own team (ICEHL only). |
#'    | xga | numeric | Expected goals against (ICEHL only; null elsewhere). |
#'    | gsax | numeric | Goals saved above expected, xGA minus goals against (ICEHL only). |
#'    | xs | numeric | Expected saves (ICEHL only; unverified). |
#'    | gate_zone_ur_goals_against | integer | Goals allowed in that net zone. |
#'    | gate_zone_ur_shots_against | integer | Shots on goal faced in that net zone. |
#'    | gate_zone_ur_saves | integer | Saves in that net zone. |
#'    | gate_zone_ur_saves_percent | numeric | Save percentage x 100 in that net zone. |
#'    | gate_zone_ul_goals_against | integer | Goals allowed in that net zone. |
#'    | gate_zone_ul_shots_against | integer | Shots on goal faced in that net zone. |
#'    | gate_zone_ul_saves | integer | Saves in that net zone. |
#'    | gate_zone_ul_saves_percent | numeric | Save percentage x 100 in that net zone. |
#'    | gate_zone_br_goals_against | integer | Goals allowed in that net zone. |
#'    | gate_zone_br_shots_against | integer | Shots on goal faced in that net zone. |
#'    | gate_zone_br_saves | integer | Saves in that net zone. |
#'    | gate_zone_br_saves_percent | numeric | Save percentage x 100 in that net zone. |
#'    | gate_zone_fh_goals_against | integer | Goals allowed in that net zone. |
#'    | gate_zone_fh_shots_against | integer | Shots on goal faced in that net zone. |
#'    | gate_zone_fh_saves | integer | Saves in that net zone. |
#'    | gate_zone_fh_saves_percent | numeric | Save percentage x 100 in that net zone. |
#'    | gate_zone_bl_goals_against | integer | Goals allowed in that net zone. |
#'    | gate_zone_bl_shots_against | integer | Shots on goal faced in that net zone. |
#'    | gate_zone_bl_saves | integer | Saves in that net zone. |
#'    | gate_zone_bl_saves_percent | numeric | Save percentage x 100 in that net zone. |
#'    | gate_zone_ce_goals_against | integer | Goals allowed in that net zone. |
#'    | gate_zone_ce_shots_against | integer | Shots on goal faced in that net zone. |
#'    | gate_zone_ce_saves | integer | Saves in that net zone. |
#'    | gate_zone_ce_saves_percent | numeric | Save percentage x 100 in that net zone. |
#'    | rank | integer | Rank among the game's goalies; null in DEL and EIHL. |
#'    | team_time_on_ice | integer | The team's total game time, seconds. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [ICE Hockey League](https://www.ice.hockey/) (the league site whose widgets read this public eSports bucket)
#' @keywords ICE Hockey League eSports
#' @family ICE Hockey League eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(icehl_goalie_box(match_id = 8161))
#' }
icehl_goalie_box <- function(match_id, return_parsed = TRUE) {
  .esports_goalie_box("icehl", match_id = match_id, return_parsed = return_parsed)
}

#' @title **ICE Hockey League Standings**
#' @description Standings of one competition season, one row per team.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/tables/{season}/{league_id}.json` of the ICE Hockey League eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/tables/2026/1.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param league_id Competition id. 1 = ICE Hockey League, regular season and playoffs in one file (split them by league_part_name); 2 = pre-season (2025/26 on).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | group_id | integer | Group id (group key space); null when the stage has no groups, so unobserved here. |
#'    | group_name | character | Group name; empty string when the stage has no groups. |
#'    | complete | logical | True when the standings include every finished game. |
#'    | team_id | integer | Team id (per-bucket team key space); joins to home_id / guest_id in the schedule. |
#'    | part_team_id | integer | Team-in-league-part id (its own key space: one team in one league part); no other captured file carries it. |
#'    | name | character | Team display name. |
#'    | team_original | character | Team's registered name. |
#'    | shortcut | character | Three-letter team code. |
#'    | position | integer | Table position, 1 = first. |
#'    | points | integer | Table points. |
#'    | games | integer | Games played. |
#'    | wins | integer | Wins in regulation (DEL, EIHL, Denmark) or all wins (ICEHL, see `winsReg`); definition per bucket unverified. |
#'    | wins_ot | integer | Wins after overtime or shootout. |
#'    | wins_reg | integer | Wins in regulation time. |
#'    | ties | integer | Ties (0 under current rules). |
#'    | losts_ot | integer | Losses after overtime or shootout. |
#'    | losts | integer | Losses in regulation time. |
#'    | score1 | integer | Goals the team scored (goals for). |
#'    | score2 | integer | Goals against. |
#'    | goals_away | integer | Goals scored in away games. |
#'    | points_away | integer | Table points from away games. |
#'    | last10 | character | Form over the last ten games as a result string; empty string in the samples. |
#'    | only_ot_wins | integer | Wins in overtime. |
#'    | only_so_wins | integer | Wins in a shootout. |
#'    | only_ot_losts | integer | Losses in overtime. |
#'    | only_so_losts | integer | Losses in a shootout. |
#'    | qualified | logical | True when the team has clinched qualification. |
#'    | live | logical | True while the team is playing a live game. |
#'    | live_game_score | character | Score of the live game; null when none, so unobserved here. |
#'    | position_change | integer | Change in table position since the previous round. |
#'    | ppg | numeric | Points per game. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [ICE Hockey League](https://www.ice.hockey/) (the league site whose widgets read this public eSports bucket)
#' @keywords ICE Hockey League eSports
#' @family ICE Hockey League eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(icehl_standings(season = 2027, league_id = 1))
#' }
icehl_standings <- function(season, league_id = 1L, return_parsed = TRUE) {
  .esports_standings("icehl", season = season, league_id = league_id, return_parsed = return_parsed)
}

#' @title **ICE Hockey League Shots**
#' @description Shots of one match with rink coordinates and result, one row per shot.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/visualization/shots/{match_id}.json` of the ICE Hockey League eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/visualization/shots/8161.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in icehl_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | match_id | integer | Match id (per-bucket match key space); joins to the `{match_id}` path param of the game wrappers. |
#'    | date | character | Game date `YYYY-MM-DD`. |
#'    | home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | home_name | character | Team display name. |
#'    | home_shortcut | character | Three-letter team code. |
#'    | visitor_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | visitor_name | character | Team display name. |
#'    | visitor_shortcut | character | Three-letter team code. |
#'    | status | character | Match status: `AFTER_MATCH`, `LIVE`, `BEFORE_MATCH`. |
#'    | shot_id | integer | Shot id within the shot files. |
#'    | player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | jersey | integer | Jersey number. |
#'    | first_name | character | First name of the shooter. |
#'    | last_name | character | Last name of the shooter. |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | match_shot_resutl_id | integer | Shot result code (the provider's typo kept): `4` = goal; `1` / `2` / `3` probably on goal / missed / blocked (unverified). A code, not a joinable id. |
#'    | time | integer | Seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | coordinate_x | numeric | Shot x along the rink LENGTH, centre ice = 0, about -100..100 (units unverified, plausibly percent of half-length). Normalised to the attacking direction: each team keeps one sign for the whole game, the home team positive in the three sample games. DEL sends two-decimal floats, ICEHL and EIHL whole numbers (`-71.0`). |
#'    | coordinate_y | numeric | Shot y across the rink WIDTH, centre line = 0, about -100..100 (units and which board is positive unverified). DEL two-decimal floats, ICEHL and EIHL whole numbers. |
#'    | real_date | character | Wall-clock local time, `YYYY-MM-DD HH:MM:SS`. |
#'    | xg | numeric | Expected-goals value of the shot (ICEHL and EIHL only; null elsewhere). |
#'    | xg_type | character | Shot-danger class from the xG model, e.g. LD / MD / HD (ICEHL and EIHL only; null elsewhere). |
#'    | shot_result_id | integer | Shot result code, the same value as match_shot_resutl_id: 4 = goal (verified); 1, 2 and 3 (on goal / missed / blocked in the widgets) are unverified. |
#'    | is_goal | logical | True when the shot result code is 4 (goal). |
#'    | team_side | character | Team of the row: home or visitor. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [ICE Hockey League](https://www.ice.hockey/) (the league site whose widgets read this public eSports bucket)
#' @keywords ICE Hockey League eSports
#' @family ICE Hockey League eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(icehl_shots(match_id = 8161))
#' }
icehl_shots <- function(match_id, return_parsed = TRUE) {
  .esports_shots("icehl", match_id = match_id, return_parsed = return_parsed)
}

#' @title **ICE Hockey League Game Center**
#' @description Game-center bundle of one match as one row: game data, team stats, goal and penalty lists.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/widget/esports/match/{match_id}.json` of the ICE Hockey League eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/widget/esports/match/8161.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in icehl_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | status_id | integer | Widget API status code, a code and not a joinable id: `1` = OK. |
#'    | status_msg | character | Widget API status text, `Ok`. |
#'    | data_game_data_id | integer | Match id (match key space, per bucket); joins to `league-matches` `matches[].id` and the `{match_id}` path param. |
#'    | data_game_data_division_id | integer | League part id under the widget's name `divisionId` (league-part key space, per bucket: one stage of a competition, e.g. ICEHL 56 = 2026/27 regular season); joins to `league_part_id` in league-matches and the `{league_part_id}` path param. |
#'    | data_game_data_division_longname | character | League part name, e.g. `ICEHL 2026/27`. |
#'    | data_game_data_division_shortname | character | League part name, e.g. `ICEHL 2026/27`. |
#'    | data_game_data_scheduled_date_formatted_short | character | Short display text `DD.MM.YY` (plus `HH:MM` for times). |
#'    | data_game_data_scheduled_date_formatted_long | character | Long display text, e.g. `18.September. 2026`. |
#'    | data_game_data_scheduled_date_timestamp | numeric | Epoch milliseconds. |
#'    | data_game_data_scheduled_date_value | character | ISO text `YYYY-MM-DD` (plus ` HH:MM` for times). |
#'    | data_game_data_scheduled_date_diff_to_now | integer | Difference to now as computed by the widget server; `0` in the samples. |
#'    | data_game_data_scheduled_time | character | Local time `HH:MM` (scheduled face-off, actual start, actual end). |
#'    | data_game_data_start_time | character | Local time `HH:MM` (scheduled face-off, actual start, actual end). |
#'    | data_game_data_end_time | character | Local time `HH:MM` (scheduled face-off, actual start, actual end). |
#'    | data_game_data_is_overtime | integer | Overtime flag sent as an integer, `1` = yes. |
#'    | data_game_data_is_shoot_out | integer | Shootout flag sent as an integer, `1` = yes. |
#'    | data_game_data_home_team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | data_game_data_home_team_longname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
#'    | data_game_data_home_team_shortname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
#'    | data_game_data_home_team_flavourname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
#'    | data_game_data_home_team_score | integer | Final goals of the home or away team (by name). |
#'    | data_game_data_away_team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | data_game_data_away_team_longname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
#'    | data_game_data_away_team_shortname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
#'    | data_game_data_away_team_flavourname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
#'    | data_game_data_away_team_score | integer | Final goals of the home or away team (by name). |
#'    | data_game_data_game_status | integer | Game status code; `4` = finished in the samples (other codes unverified). |
#'    | data_game_data_game_extended_status | integer | Extended status code; `0` in the samples. |
#'    | data_game_data_game_extended_status_info_text | character | Extended status text; empty string in the samples. |
#'    | data_game_data_game_name | character | Game number sent as a string (e.g. `"66"`). |
#'    | data_game_data_game_day | character | Matchday number sent as a string (`"1"`); null in Denmark. |
#'    | data_game_data_game_round | integer | Round number; `0` in the samples. |
#'    | data_game_data_location_longname | character | Arena name; empty string when not set. |
#'    | data_game_data_location_shortname | character | Arena name; empty string when not set. |
#'    | data_game_data_location_coordinates_lat | integer | Latitude / longitude; `0` placeholder in the samples. |
#'    | data_game_data_location_coordinates_lng | integer | Latitude / longitude; `0` placeholder in the samples. |
#'    | data_game_data_attendance | integer | Spectators at the game (attendance). |
#'    | data_game_data_live_time | integer | Game clock, seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). 3600 at the end of regulation. |
#'    | data_game_data_live_time_formatted | character | Clock text; the placeholder `END-GAME` after the game, `m:ss` while live. |
#'    | data_game_data_live_time_period | integer | Current period number; `4` after the game. |
#'    | data_game_data_live_time_string | character | Clock text; the placeholder `END-GAME` after the game, `m:ss` while live. |
#'    | data_game_data_live_time_game_phase | character | Game phase text, e.g. `END OF GAME`. |
#'    | data_game_data_period_stats | character | Per-period score and shots. |
#'    | data_game_data_game_officials_ref1_lastname | character | Last name of that official or coach. |
#'    | data_game_data_game_officials_ref1_firstname | character | First name of that official or coach. |
#'    | data_game_data_game_officials_ref2_lastname | character | Last name of that official or coach. |
#'    | data_game_data_game_officials_ref2_firstname | character | First name of that official or coach. |
#'    | data_game_data_game_officials_linesman1_lastname | character | Last name of that official or coach. |
#'    | data_game_data_game_officials_linesman1_firstname | character | First name of that official or coach. |
#'    | data_game_data_game_officials_linesman2_lastname | character | Last name of that official or coach. |
#'    | data_game_data_game_officials_linesman2_firstname | character | First name of that official or coach. |
#'    | data_game_data_team_officials_headcoachhome_lastname | character | Last name of that official or coach. |
#'    | data_game_data_team_officials_headcoachhome_firstname | character | First name of that official or coach. |
#'    | data_game_data_team_officials_headcoachaway_lastname | character | Last name of that official or coach. |
#'    | data_game_data_team_officials_headcoachaway_firstname | character | First name of that official or coach. |
#'    | data_game_data_live_time_period_bar | character | Period progress bar; null in the samples, so unobserved. |
#'    | data_game_data_you_tube_link | character | Video link; null in the samples, so unobserved. |
#'    | data_game_data_number_of_periods | integer | Regulation periods (3) or the maximum overtimes allowed. |
#'    | data_game_data_length_of_period | integer | Length of a period or overtime, seconds (1200). |
#'    | data_game_data_number_of_overtimes | integer | Regulation periods (3) or the maximum overtimes allowed. |
#'    | data_game_data_length_of_overtime | integer | Length of a period or overtime, seconds (1200). |
#'    | data_game_data_shootout_shots | integer | Shootout attempts in the game. |
#'    | data_game_data_images_home_team_logo | character | Team logo; null in the samples, so unobserved. |
#'    | data_game_data_images_away_team_logo | character | Team logo; null in the samples, so unobserved. |
#'    | data_game_data_streams | character | Video streams; empty array in the samples. |
#'    | data_game_data_home_team_logo_url | character | Team logo URL; null in the samples, so unobserved. |
#'    | data_game_data_away_team_logo_url | character | Team logo URL; null in the samples, so unobserved. |
#'    | data_home_goals | character | Goals scored by that team. |
#'    | data_home_penalties | character | Penalties of that team. |
#'    | data_home_field_players | character | Skaters of that team with their game stats. |
#'    | data_home_goal_keepers | character | Goalies of that team with their game stats. |
#'    | data_home_team_stats_faceoffs_won | integer | Face-offs the team won in this game. |
#'    | data_home_team_stats_faceoffs_total | integer | Face-offs the team took in this game. |
#'    | data_home_team_stats_shots_on_goal | integer | Team shots that reached the goalie in this game. |
#'    | data_home_team_stats_shots_blocked | integer | Team blocked-shot count in this game (direction unverified). |
#'    | data_home_team_stats_penalty_minutes | integer | Penalty minutes assessed to the team in this game. |
#'    | data_home_team_stats_powerplay_goals | integer | Goals the team scored on the power play in this game. |
#'    | data_home_team_stats_powerplay_opportunities | integer | Power plays the team had in this game. |
#'    | data_home_team_stats_penaltykill_successes | integer | Penalties killed without conceding. |
#'    | data_home_team_stats_penaltykill_situations | integer | Times the team had to kill a penalty in this game. |
#'    | data_home_team_stats_time_values_leading | integer | Seconds spent leading, tied or trailing (by name). |
#'    | data_home_team_stats_time_values_tied | integer | Seconds spent leading, tied or trailing (by name). |
#'    | data_home_team_stats_time_values_trailing | integer | Seconds spent leading, tied or trailing (by name). |
#'    | data_home_team_stats_time_values_time_game_strength_even | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_home_team_stats_time_values_time_game_strength_plus_one | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_home_team_stats_time_values_time_game_strength_plus_two | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_home_team_stats_time_values_time_game_strength_minus_one | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_home_team_stats_time_values_time_game_strength_minus_two | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_home_team_stats_time_values_time_on_powerplay | integer | Seconds on the power play. |
#'    | data_home_team_stats_time_values_time_while_shorthanded | integer | Seconds the team played shorthanded in this game. |
#'    | data_away_goals | character | Goals scored by that team. |
#'    | data_away_penalties | character | Penalties of that team. |
#'    | data_away_field_players | character | Skaters of that team with their game stats. |
#'    | data_away_goal_keepers | character | Goalies of that team with their game stats. |
#'    | data_away_team_stats_faceoffs_won | integer | Face-offs the team won in this game. |
#'    | data_away_team_stats_faceoffs_total | integer | Face-offs the team took in this game. |
#'    | data_away_team_stats_shots_on_goal | integer | Team shots that reached the goalie in this game. |
#'    | data_away_team_stats_shots_blocked | integer | Team blocked-shot count in this game (direction unverified). |
#'    | data_away_team_stats_penalty_minutes | integer | Penalty minutes assessed to the team in this game. |
#'    | data_away_team_stats_powerplay_goals | integer | Goals the team scored on the power play in this game. |
#'    | data_away_team_stats_powerplay_opportunities | integer | Power plays the team had in this game. |
#'    | data_away_team_stats_penaltykill_successes | integer | Penalties killed without conceding. |
#'    | data_away_team_stats_penaltykill_situations | integer | Times the team had to kill a penalty in this game. |
#'    | data_away_team_stats_time_values_leading | integer | Seconds spent leading, tied or trailing (by name). |
#'    | data_away_team_stats_time_values_tied | integer | Seconds spent leading, tied or trailing (by name). |
#'    | data_away_team_stats_time_values_trailing | integer | Seconds spent leading, tied or trailing (by name). |
#'    | data_away_team_stats_time_values_time_game_strength_even | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_away_team_stats_time_values_time_game_strength_plus_one | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_away_team_stats_time_values_time_game_strength_plus_two | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_away_team_stats_time_values_time_game_strength_minus_one | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_away_team_stats_time_values_time_game_strength_minus_two | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_away_team_stats_time_values_time_on_powerplay | integer | Seconds on the power play. |
#'    | data_away_team_stats_time_values_time_while_shorthanded | integer | Seconds the team played shorthanded in this game. |
#'    | data_home_goal_keeper_changes | character | Goalie on/off events of that team. |
#'    | data_away_goal_keeper_changes | character | Goalie on/off events of that team. |
#'    | data_game_actions | character | Empty array in both samples; item shape unobserved. |
#'    | data_faceoffs_head_to_head | character | Empty array in both samples; item shape unobserved. |
#'    | data_faceoffs_individual | character | Empty array in both samples; item shape unobserved. |
#'    | data_game_situations | character | Empty array in both samples; item shape unobserved. |
#'    | data_shootouts | character | Empty array in both samples; item shape unobserved. |
#'    | calc_time | integer | Server computation time; `0` in the samples. |
#'    | last_update_formatted_short | character | Short display text `DD.MM.YY` (plus `HH:MM` for times). |
#'    | last_update_formatted_long | character | Long display text, e.g. `18.September. 2026`. |
#'    | last_update_timestamp | numeric | Epoch milliseconds. |
#'    | last_update_value | character | ISO text `YYYY-MM-DD` (plus ` HH:MM` for times). |
#'    | served_by_server | character | Host that built the bundle, e.g. `icehl.hokejovyzapis.cz`. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [ICE Hockey League](https://www.ice.hockey/) (the league site whose widgets read this public eSports bucket)
#' @keywords ICE Hockey League eSports
#' @family ICE Hockey League eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(icehl_game_center(match_id = 8161))
#' }
icehl_game_center <- function(match_id, return_parsed = TRUE) {
  .esports_game_center("icehl", match_id = match_id, return_parsed = return_parsed)
}

#' @title **EIHL Schedule**
#' @description Schedule and results of one competition season, one row per match, played or not.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/league-matches/{season}/{league_id}.json` of the EIHL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/league-matches/2026/3.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param league_id Competition id. 3 = Elite League, 4 = Challenge Cup, 5 = playoffs.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | match_id | integer | Match id (per-bucket match key space); joins to the `{match_id}` path param of the game wrappers. |
#'    | start_date | character | Scheduled face-off, local `YYYY-MM-DD HH:MM:SS`; DEL playoff placeholders read `0000-00-00 ...`. |
#'    | league_id | integer | League id (per-bucket competition key space); joins to the `{league_id}` path param. |
#'    | league | character | League display name, e.g. `DEL`, `EIHL`, `Metal Ligaen`. |
#'    | season | integer | Season by its END year (2027 = 2026/27); the body's START-year key plus one. |
#'    | round | character | Round number as a string (`"1"`, `"249"`); DEL values carry a leading U+FEFF byte-order mark, empty string in Denmark. |
#'    | group | character | Group name in group stages; empty string or null otherwise. |
#'    | home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | home_name | character | Team display name. |
#'    | home_shortcut | character | Three-letter team code. |
#'    | guest_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | guest_name | character | Team display name. |
#'    | guest_shortcut | character | Three-letter team code. |
#'    | status | character | Match status: `BEFORE_MATCH`, `LIVE`, `AFTER_MATCH`. |
#'    | overtime_count | integer | Number of overtime periods played; null in the samples. |
#'    | extra_time | logical | True when the game went to overtime. |
#'    | shooting | logical | True when the game was decided by a shootout. |
#'    | contumacy | logical | True when the result was awarded by forfeit. |
#'    | final_score_home | integer | Home team goals in that span. |
#'    | final_score_guest | integer | Visiting team goals in that span. |
#'    | first_period_score_home | integer | Home team goals in that span. |
#'    | first_period_score_guest | integer | Visiting team goals in that span. |
#'    | second_period_score_home | integer | Home team goals in that span. |
#'    | second_period_score_guest | integer | Visiting team goals in that span. |
#'    | third_period_score_home | integer | Home team goals in that span. |
#'    | third_period_score_guest | integer | Visiting team goals in that span. |
#'    | actual_time_name | character | Display label of the game state in the bucket's language, e.g. `end`, `Ende`, `before game`. |
#'    | actual_time_alias | character | Short game-state code: `K` = finished (Czech `konec`), `0` = not started. |
#'    | last_event_time | integer | Game second of the latest event, of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | arena | character | Arena name; empty string or null where not set. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [EIHL](https://www.eliteleague.co.uk/) (the league site whose widgets read this public eSports bucket)
#' @keywords EIHL eSports
#' @family EIHL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(eihl_schedule(season = 2027, league_id = 3))
#' }
eihl_schedule <- function(season, league_id = 3L, return_parsed = TRUE) {
  .esports_schedule("eihl", season = season, league_id = league_id, return_parsed = return_parsed)
}

#' @title **EIHL Team Player Stats**
#' @description Season player stats of one team in one competition, one row per player.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/league-team-stats/{season}/{league_id}/{team_id}.json` of the EIHL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/league-team-stats/2026/3/96.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param team_id Team id (per-bucket team key space: home_id / guest_id in eihl_schedule).
#' @param league_id Competition id. 3 = Elite League, 4 = Challenge Cup, 5 = playoffs.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (playerId key space, per bucket, stable across games and seasons). |
#'    | name | character | Player full name. |
#'    | firstname | character | First name of the player in this row. |
#'    | surname | character | Player last name. |
#'    | birthdate | character | Birth date `YYYY-MM-DD`. |
#'    | active | logical | True when the player is active on the team. |
#'    | position | character | Position code: `GK` goalie, `DE` defence, `FW` / `FO` forward (FO meaning unverified). |
#'    | stick | character | Handedness: `left` or `right`; null when unknown. |
#'    | weight | integer | Weight in kg; `0` placeholder when unknown. |
#'    | height | integer | Height in cm; `0` placeholder when unknown. |
#'    | jersey | integer | Jersey number. |
#'    | birth_country | character | Player's country of birth (name). |
#'    | nationality | character | Nationality, country name. |
#'    | nationality_short | character | Nationality, three-letter code (`CAN`, `GER`; `UK` for Britain). |
#'    | team_shortcut | character | Three-letter team code. |
#'    | games | integer | Games played. |
#'    | goals | integer | goals in total (goals_home + goals_away: the body splits it by home and away games). |
#'    | goals_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | goals_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists | integer | assists in total (assists_home + assists_away: the body splits it by home and away games). |
#'    | assists_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points | integer | points in total (points_home + points_away: the body splits it by home and away games). |
#'    | points_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | penalty_minutes | integer | Penalty minutes assessed to the player (this game, or to date in the season / league-part files). |
#'    | positive | integer | Plus: on ice for goals for at even strength or shorthanded. |
#'    | negative | integer | Minus: on ice for goals against at even strength or on the power play. |
#'    | points_per_game | numeric | Average goals plus assists per game played. |
#'    | shot_efficiency | numeric | Shooting percentage, goals / shots on goal x 100. |
#'    | penalty_minutes_per_game | numeric | Average penalty minutes per game played. |
#'    | pp_goals | integer | Power-play goals. |
#'    | sh_goals | integer | Shorthanded goals. |
#'    | gw_goals | integer | Game-winning goals. |
#'    | shots_attempts | integer | Shot attempts (on goal + missed + blocked). |
#'    | shots_on_goal | integer | shots_on_goal in total (shots_on_goal_home + shots_on_goal_away: the body splits it by home and away games). |
#'    | shots_on_goal_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_on_goal_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_missed | integer | This player's shot attempts that missed the net. |
#'    | shots_blocked | integer | This player's shot attempts that were blocked. |
#'    | blocked_shots_by_player | integer | Opponent shots this player blocked. |
#'    | faceoffs_win | integer | Face-offs the player won (this game, or to date in the season / league-part files). |
#'    | faceoffs_losses | integer | Face-offs the player lost (this game, or to date in the season / league-part files). |
#'    | faceoffs_count | integer | Face-offs the player took (this game, or to date in the season / league-part files). |
#'    | timeonice | numeric | Time on ice, seconds with decimals (tracking-based; differs from `timeOnIce`). |
#'    | timeonice_eq | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | timeonice_pp | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | timeonice_sh | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | hits | integer | Body checks delivered. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [EIHL](https://www.eliteleague.co.uk/) (the league site whose widgets read this public eSports bucket)
#' @keywords EIHL eSports
#' @family EIHL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(eihl_team_player_stats(season = 2027, team_id = 96, league_id = 3))
#' }
eihl_team_player_stats <- function(season, team_id, league_id = 3L, return_parsed = TRUE) {
  .esports_team_player_stats("eihl", season = season, team_id = team_id, league_id = league_id, return_parsed = return_parsed)
}

#' @title **EIHL Top Scorers**
#' @description Skater scoring leaderboard of one competition season.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/league-top-scorers/{season}/{league_id}.json` of the EIHL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/league-top-scorers/2026/3.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param league_id Competition id. 3 = Elite League, 4 = Challenge Cup, 5 = playoffs.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | name | character | Player first name. |
#'    | surname | character | Player last name. |
#'    | jersey | integer | Jersey number. |
#'    | birthdate | character | Birth date `YYYY-MM-DD`. |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_name | character | Team display name. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | matches | integer | Games played (two names for the same count). |
#'    | goals | integer | Goals the player scored in the league-season. |
#'    | assists | integer | Assists credited to the player in the league-season. |
#'    | points | integer | Points, goals + assists. |
#'    | shots | integer | Shots on goal. |
#'    | positives | integer | Plus events of the player: on ice for a goal for (league-season). |
#'    | negatives | integer | Minus events of the player: on ice for a goal against (league-season). |
#'    | positives_negatives | integer | Plus/minus, positives - negatives. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [EIHL](https://www.eliteleague.co.uk/) (the league site whose widgets read this public eSports bucket)
#' @keywords EIHL eSports
#' @family EIHL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(eihl_top_scorers(season = 2027, league_id = 3))
#' }
eihl_top_scorers <- function(season, league_id = 3L, return_parsed = TRUE) {
  .esports_top_scorers("eihl", season = season, league_id = league_id, return_parsed = return_parsed)
}

#' @title **EIHL Game Info**
#' @description Game info of one match: state, arena, attendance, officials, coaches, period scores.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/matches/{match_id}/game-info.json` of the EIHL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/matches/4440/game-info.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in eihl_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | actual_time_name | character | Display label of the game state, e.g. `end` / `Ende`. |
#'    | actual_time_alias | character | Short game-state code: `K` = finished. |
#'    | stadium | character | Name of the arena the game was played in. |
#'    | number_of_viewers | integer | Spectators at the game (attendance). |
#'    | referees_head_referee1 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | referees_head_referee2 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | referees_line_referee1 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | referees_line_referee2 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | team_info_home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_info_home_name | character | Team display name. |
#'    | team_info_home_shortcut | character | Three-letter team code. |
#'    | team_info_visitor_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_info_visitor_name | character | Team display name. |
#'    | team_info_visitor_shortcut | character | Three-letter team code. |
#'    | trainers_home_head_coach | character | Head coach (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | trainers_visitor_head_coach | character | Head coach (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | team_bilance_home_win | integer | Wins of that team in the head-to-head record (unverified). |
#'    | team_bilance_visitor_win | integer | Wins of that team in the head-to-head record (unverified). |
#'    | results_extra_time | logical | True when the game went to overtime. |
#'    | results_shooting | logical | True when the game was decided by a shootout. |
#'    | results_contumacy | logical | True when the result was awarded by forfeit. |
#'    | results_score_final_score_home | integer | Home team goals in that span. |
#'    | results_score_final_score_guest | integer | Visiting team goals in that span. |
#'    | results_score_first_period_score_home | integer | Home team goals in that span. |
#'    | results_score_first_period_score_guest | integer | Visiting team goals in that span. |
#'    | results_score_second_period_score_home | integer | Home team goals in that span. |
#'    | results_score_second_period_score_guest | integer | Visiting team goals in that span. |
#'    | results_score_third_period_score_home | integer | Home team goals in that span. |
#'    | results_score_third_period_score_guest | integer | Visiting team goals in that span. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [EIHL](https://www.eliteleague.co.uk/) (the league site whose widgets read this public eSports bucket)
#' @keywords EIHL eSports
#' @family EIHL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(eihl_game_info(match_id = 4440))
#' }
eihl_game_info <- function(match_id, return_parsed = TRUE) {
  .esports_game_info("eihl", match_id = match_id, return_parsed = return_parsed)
}

#' @title **EIHL Pbp**
#' @description Play-by-play of one match: goals, penalties, goalie changes and timeouts, one row per event.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/matches/{match_id}/period-events.json` of the EIHL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/matches/4440/period-events.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in eihl_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | event_index | integer | Row order of the event within the game (0-based), across periods. |
#'    | period | character | Period key as the body sends it: 1, 2, 3, OT (overtime) or SO (shootout). |
#'    | period_number | integer | Period number: 1-3, 4 = overtime, 5 = shootout. |
#'    | time | integer | Game second of the event from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | event_type | character | Event type: goal, penalty, goalkeeperChange, timeout, periodStart, periodEnd, videoReview, ... |
#'    | event_id | integer | Event id within its event type. |
#'    | team_side | character | Team of the row: home or visitor. |
#'    | goalie_player_id | integer | Player id (playerId key space) of the goalie entering the game (goalkeeperChange). |
#'    | goalie_jersey | integer | Jersey number of the goalie entering the game (goalkeeperChange). |
#'    | goalie_name | character | Full name of the goalie entering the game (goalkeeperChange). |
#'    | current_score | character | Score after the goal, as `home:visitor` text, e.g. `1:0`. |
#'    | real_time | character | Wall-clock local time, `YYYY-MM-DD HH:MM:SS`. |
#'    | scoreboard_time | integer | Seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | balance | character | Goal strength: `EQ` even, `PP1` / `PP2` power play, `SH1` / `SH2` shorthanded. |
#'    | extra_attacker | logical | True when the scoring team had an extra attacker on (goalie pulled). |
#'    | empty_net | logical | True when the goal went into an empty net. |
#'    | shooting | logical | True for a goal scored on a penalty shot. |
#'    | technical | logical | True for a technical penalty (unverified). |
#'    | scorer_player_id | integer | Player id (playerId key space) of the goal scorer. |
#'    | scorer_jersey | integer | Jersey number of the goal scorer. |
#'    | scorer_name | character | Full name of the goal scorer. |
#'    | assist1_player_id | integer | Player id (playerId key space) of the first assist. |
#'    | assist1_jersey | integer | Jersey number of the first assist. |
#'    | assist1_name | character | Full name of the first assist. |
#'    | assist2_player_id | integer | Player id (playerId key space) of the second assist. |
#'    | assist2_jersey | integer | Jersey number of the second assist. |
#'    | assist2_name | character | Full name of the second assist. |
#'    | plus_player_ids | character | Comma-separated player ids (playerId key space) of the scoring team's players on ice. |
#'    | minus_player_ids | character | Comma-separated player ids (playerId key space) of the conceding team's players on ice. |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | goal_review | character | Video review subject, e.g. a goal (EIHL `videoReview` events). |
#'    | on_ice_decision | character | Call on the ice before the review. |
#'    | reason_for_review | character | Why the play was reviewed (free text). |
#'    | decision_after_review | character | Final call after the review. |
#'    | outgoing_goalie_player_id | integer | Player id (playerId key space) of the goalie leaving the game (goalkeeperChange). |
#'    | outgoing_goalie_jersey | integer | Jersey number of the goalie leaving the game (goalkeeperChange). |
#'    | outgoing_goalie_name | character | Full name of the goalie leaving the game (goalkeeperChange). |
#'    | create_time | integer | Game second at which the event was entered (unverified). |
#'    | penalized_player_id | integer | Player id (playerId key space) of the penalized player. |
#'    | penalized_jersey | integer | Jersey number of the penalized player. |
#'    | penalized_name | character | Full name of the penalized player. |
#'    | penalty_from_real_time | character | Penalty start, local wall clock YYYY-MM-DD HH:MM:SS. |
#'    | penalty_from_scoreboard_time | integer | Penalty start, game second. |
#'    | penalty_to_real_time | character | Penalty end, local wall clock YYYY-MM-DD HH:MM:SS. |
#'    | penalty_to_scoreboard_time | integer | Penalty end, game second. |
#'    | duration | integer | Penalty length in seconds (120 = minor). |
#'    | disciplined_functionary | logical | True when the penalty was assessed to a team official. |
#'    | disciplined_bm | logical | True when the penalty was a bench minor. |
#'    | codename | character | Penalty code, e.g. `HOOK`, `TRIP`, `ROUGH`, `BOARD`, `DELAY`. |
#'    | replaced_player_id | integer | Player id (playerId key space) of the player serving the penalty in the penalized player's place. |
#'    | replaced_jersey | integer | Jersey number of the player serving the penalty in the penalized player's place. |
#'    | replaced_name | character | Full name of the player serving the penalty in the penalized player's place. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [EIHL](https://www.eliteleague.co.uk/) (the league site whose widgets read this public eSports bucket)
#' @keywords EIHL eSports
#' @family EIHL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(eihl_pbp(match_id = 4440))
#' }
eihl_pbp <- function(match_id, return_parsed = TRUE) {
  .esports_pbp("eihl", match_id = match_id, return_parsed = return_parsed)
}

#' @title **EIHL Player Box**
#' @description Player boxscore of one team in one match, one row per player.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/matches/{match_id}/team-stats/{team_id}.json` of the EIHL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/matches/4440/team-stats/96.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in eihl_schedule). Not the league website's game id.
#' @param team_id Team id (per-bucket team key space: home_id / guest_id in eihl_schedule).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (playerId key space, per bucket, stable across games and seasons). |
#'    | name | character | Player full name. |
#'    | firstname | character | First name of the player in this row. |
#'    | surname | character | Player last name. |
#'    | birthdate | character | Birth date `YYYY-MM-DD`. |
#'    | active | logical | True when the player is active on the team. |
#'    | position | character | Position code: `GK` goalie, `DE` defence, `FW` / `FO` forward (FO meaning unverified). |
#'    | stick | character | Handedness: `left` or `right`; null when unknown. |
#'    | weight | integer | Weight in kg; `0` placeholder when unknown. |
#'    | height | integer | Height in cm; `0` placeholder when unknown. |
#'    | jersey | integer | Jersey number. |
#'    | birth_country | character | Player's country of birth (name). |
#'    | nationality | character | Nationality, country name. |
#'    | nationality_short | character | Nationality, three-letter code (`CAN`, `GER`; `UK` for Britain). |
#'    | team_shortcut | character | Three-letter team code. |
#'    | games | integer | Games played. |
#'    | goals | integer | goals in total (goals_home + goals_away: the body splits it by home and away games). |
#'    | goals_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | goals_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists | integer | assists in total (assists_home + assists_away: the body splits it by home and away games). |
#'    | assists_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points | integer | points in total (points_home + points_away: the body splits it by home and away games). |
#'    | points_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | penalty_minutes | integer | Penalty minutes assessed to the player (this game, or to date in the season / league-part files). |
#'    | positive | integer | Plus: on ice for goals for at even strength or shorthanded. |
#'    | negative | integer | Minus: on ice for goals against at even strength or on the power play. |
#'    | points_per_game | numeric | Average goals plus assists per game played. |
#'    | shot_efficiency | numeric | Shooting percentage, goals / shots on goal x 100. |
#'    | penalty_minutes_per_game | numeric | Average penalty minutes per game played. |
#'    | pp_goals | integer | Power-play goals. |
#'    | sh_goals | integer | Shorthanded goals. |
#'    | gw_goals | integer | Game-winning goals. |
#'    | shots_attempts | integer | Shot attempts (on goal + missed + blocked). |
#'    | shots_on_goal | integer | shots_on_goal in total (shots_on_goal_home + shots_on_goal_away: the body splits it by home and away games). |
#'    | shots_on_goal_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_on_goal_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_missed | integer | This player's shot attempts that missed the net. |
#'    | shots_blocked | integer | This player's shot attempts that were blocked. |
#'    | blocked_shots_by_player | integer | Opponent shots this player blocked. |
#'    | faceoffs_win | integer | Face-offs the player won (this game, or to date in the season / league-part files). |
#'    | faceoffs_losses | integer | Face-offs the player lost (this game, or to date in the season / league-part files). |
#'    | faceoffs_count | integer | Face-offs the player took (this game, or to date in the season / league-part files). |
#'    | timeonice | numeric | Time on ice, seconds with decimals (tracking-based; differs from `timeOnIce`). |
#'    | timeonice_eq | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | timeonice_pp | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | timeonice_sh | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | hits | integer | Body checks delivered. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [EIHL](https://www.eliteleague.co.uk/) (the league site whose widgets read this public eSports bucket)
#' @keywords EIHL eSports
#' @family EIHL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(eihl_player_box(match_id = 4440, team_id = 96))
#' }
eihl_player_box <- function(match_id, team_id, return_parsed = TRUE) {
  .esports_player_box("eihl", match_id = match_id, team_id = team_id, return_parsed = return_parsed)
}

#' @title **EIHL Goalie Box**
#' @description Goalie lines of one match, one row per goalie who played.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/matches/{match_id}/top-goalies.json` of the EIHL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/matches/4440/top-goalies.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in eihl_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | name | character | Goalie first name. |
#'    | surname | character | Goalie last name. |
#'    | birthdate | character | Birth date `YYYY-MM-DD`. |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_name | character | Team display name. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | jersey | integer | Jersey number. |
#'    | matches | integer | Games played (1 in a per-game file). |
#'    | won_matches | integer | Games the goalie won. |
#'    | lost_matches | integer | Games the goalie lost. |
#'    | saves | integer | Shots on goal the goalie stopped. |
#'    | goals_against | integer | Goals the goalie allowed. |
#'    | saves_percent | numeric | Save percentage x 100 (e.g. `85.71`). |
#'    | average | numeric | Goals-against average per 60 minutes. |
#'    | zeros | integer | Shutouts by the goalie (`zeros`: games with no goal allowed). |
#'    | time_on_ice | integer | Time in goal, seconds. |
#'    | seconds_per_game | numeric | Time in goal per game, seconds. |
#'    | minutes_in_play | numeric | Share of the team's game time in goal, percent (100.0 = the whole game). |
#'    | visible_games | logical | Widget flag: show the milestone for that stat. |
#'    | visible_shutouts | logical | Widget flag: show the milestone for that stat. |
#'    | visible_wins | logical | Widget flag: show the milestone for that stat. |
#'    | stars_games | integer | Widget star marker for that stat (unverified). |
#'    | stars_shutouts | integer | Widget star marker for that stat (unverified). |
#'    | stars_wins | integer | Widget star marker for that stat (unverified). |
#'    | milestone_games | numeric | Widget value for the next milestone of that stat (unverified). |
#'    | milestone_shutouts | numeric | Widget value for the next milestone of that stat (unverified). |
#'    | milestone_wins | numeric | Widget value for the next milestone of that stat (unverified). |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [EIHL](https://www.eliteleague.co.uk/) (the league site whose widgets read this public eSports bucket)
#' @keywords EIHL eSports
#' @family EIHL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(eihl_goalie_box(match_id = 4440))
#' }
eihl_goalie_box <- function(match_id, return_parsed = TRUE) {
  .esports_goalie_box("eihl", match_id = match_id, return_parsed = return_parsed)
}

#' @title **EIHL Standings**
#' @description Standings of one competition season, one row per team.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/table/{season}/{league_id}.json` of the EIHL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/table/2026/3.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param league_id Competition id. 3 = Elite League, 4 = Challenge Cup, 5 = playoffs.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | group_id | integer | Group id (group key space); null when the stage has no groups, so unobserved here. |
#'    | group_name | character | Group name; empty string when the stage has no groups. |
#'    | complete | logical | True when the standings include every finished game. |
#'    | team_id | integer | Team id (per-bucket team key space); joins to home_id / guest_id in the schedule. |
#'    | part_team_id | integer | Team-in-league-part id (its own key space: one team in one league part); no other captured file carries it. |
#'    | name | character | Team display name. |
#'    | team_original | character | Team's registered name. |
#'    | shortcut | character | Three-letter team code. |
#'    | position | integer | Table position, 1 = first. |
#'    | points | integer | Table points. |
#'    | games | integer | Games played. |
#'    | wins | integer | Wins in regulation (DEL, EIHL, Denmark) or all wins (ICEHL, see `winsReg`); definition per bucket unverified. |
#'    | wins_ot | integer | Wins after overtime or shootout. |
#'    | wins_reg | integer | Wins in regulation time. |
#'    | ties | integer | Ties (0 under current rules). |
#'    | losts_ot | integer | Losses after overtime or shootout. |
#'    | losts | integer | Losses in regulation time. |
#'    | score1 | integer | Goals the team scored (goals for). |
#'    | score2 | integer | Goals against. |
#'    | goals_away | integer | Goals scored in away games. |
#'    | last10 | character | Form over the last ten games as a result string; empty string in the samples. |
#'    | only_ot_wins | integer | Wins in overtime. |
#'    | only_so_wins | integer | Wins in a shootout. |
#'    | only_ot_losts | integer | Losses in overtime. |
#'    | only_so_losts | integer | Losses in a shootout. |
#'    | qualified | logical | True when the team has clinched qualification. |
#'    | ppg | numeric | Points per game. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [EIHL](https://www.eliteleague.co.uk/) (the league site whose widgets read this public eSports bucket)
#' @keywords EIHL eSports
#' @family EIHL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(eihl_standings(season = 2027, league_id = 3))
#' }
eihl_standings <- function(season, league_id = 3L, return_parsed = TRUE) {
  .esports_standings("eihl", season = season, league_id = league_id, return_parsed = return_parsed)
}

#' @title **EIHL Shots**
#' @description Shots of one match with rink coordinates and result, one row per shot.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/visualization/shots/{match_id}.json` of the EIHL eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/visualization/shots/4440.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in eihl_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | match_id | integer | Match id (per-bucket match key space); joins to the `{match_id}` path param of the game wrappers. |
#'    | date | character | Game date `YYYY-MM-DD`. |
#'    | home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | home_name | character | Team display name. |
#'    | home_shortcut | character | Three-letter team code. |
#'    | visitor_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | visitor_name | character | Team display name. |
#'    | visitor_shortcut | character | Three-letter team code. |
#'    | status | character | Match status: `AFTER_MATCH`, `LIVE`, `BEFORE_MATCH`. |
#'    | shot_id | integer | Shot id within the shot files. |
#'    | player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | jersey | integer | Jersey number. |
#'    | first_name | character | First name of the shooter. |
#'    | last_name | character | Last name of the shooter. |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | match_shot_resutl_id | integer | Shot result code (the provider's typo kept): `4` = goal; `1` / `2` / `3` probably on goal / missed / blocked (unverified). A code, not a joinable id. |
#'    | time | integer | Seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | coordinate_x | numeric | Shot x along the rink LENGTH, centre ice = 0, about -100..100 (units unverified, plausibly percent of half-length). Normalised to the attacking direction: each team keeps one sign for the whole game, the home team positive in the three sample games. DEL sends two-decimal floats, ICEHL and EIHL whole numbers (`-71.0`). |
#'    | coordinate_y | numeric | Shot y across the rink WIDTH, centre line = 0, about -100..100 (units and which board is positive unverified). DEL two-decimal floats, ICEHL and EIHL whole numbers. |
#'    | real_date | character | Wall-clock local time, `YYYY-MM-DD HH:MM:SS`. |
#'    | xg | numeric | Expected-goals value of the shot (ICEHL and EIHL only; null elsewhere). |
#'    | xg_type | character | Shot-danger class from the xG model, e.g. LD / MD / HD (ICEHL and EIHL only; null elsewhere). |
#'    | shot_result_id | integer | Shot result code, the same value as match_shot_resutl_id: 4 = goal (verified); 1, 2 and 3 (on goal / missed / blocked in the widgets) are unverified. |
#'    | is_goal | logical | True when the shot result code is 4 (goal). |
#'    | team_side | character | Team of the row: home or visitor. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [EIHL](https://www.eliteleague.co.uk/) (the league site whose widgets read this public eSports bucket)
#' @keywords EIHL eSports
#' @family EIHL eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(eihl_shots(match_id = 4440))
#' }
eihl_shots <- function(match_id, return_parsed = TRUE) {
  .esports_shots("eihl", match_id = match_id, return_parsed = return_parsed)
}

#' @title **Metal Ligaen Schedule**
#' @description Schedule and results of one competition season, one row per match, played or not.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/league-matches/{season}/{league_id}.json` of the Metal Ligaen eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/league-matches/2026/1.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param league_id Competition id. 1 = Metal Ligaen, regular season and playoffs in one file (split them by league_part_name); 2 = pre-season, 4 = Final-4 cup.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | match_id | integer | Match id (per-bucket match key space); joins to the `{match_id}` path param of the game wrappers. |
#'    | start_date | character | Scheduled face-off, local `YYYY-MM-DD HH:MM:SS`; DEL playoff placeholders read `0000-00-00 ...`. |
#'    | real_start_date | character | Actual start, local `YYYY-MM-DD HH:MM:SS`; `0000-00-00 00:00:00` placeholder before the game. |
#'    | real_end_date | character | Actual end, local `YYYY-MM-DD HH:MM:SS`. |
#'    | league_id | integer | League id (per-bucket competition key space); joins to the `{league_id}` path param. |
#'    | league | character | League display name, e.g. `DEL`, `EIHL`, `Metal Ligaen`. |
#'    | league_part_id | integer | League part id (league-part key space, per bucket: one stage of a competition, e.g. ICEHL 56 = 2026/27 regular season); joins to `league_part_id` in league-matches and the `{league_part_id}` path param. |
#'    | league_part_name | character | League part name, e.g. `ICEHL 2026/27` or `... - Playoffs`; splits regular season from playoffs where one league id holds both. |
#'    | season | integer | Season by its END year (2027 = 2026/27); the body's START-year key plus one. |
#'    | round | character | Round number as a string (`"1"`, `"249"`); DEL values carry a leading U+FEFF byte-order mark, empty string in Denmark. |
#'    | group | character | Group name in group stages; empty string or null otherwise. |
#'    | tickets_url | character | URL of the ticket shop for the match. |
#'    | home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | home_name | character | Team display name. |
#'    | home_shortcut | character | Three-letter team code. |
#'    | guest_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | guest_name | character | Team display name. |
#'    | guest_shortcut | character | Three-letter team code. |
#'    | status | character | Match status: `BEFORE_MATCH`, `LIVE`, `AFTER_MATCH`. |
#'    | overtime_count | integer | Number of overtime periods played; null in the samples. |
#'    | extra_time | logical | True when the game went to overtime. |
#'    | shooting | logical | True when the game was decided by a shootout. |
#'    | contumacy | logical | True when the result was awarded by forfeit. |
#'    | final_score_home | integer | Home team goals in that span. |
#'    | final_score_guest | integer | Visiting team goals in that span. |
#'    | first_period_score_home | integer | Home team goals in that span. |
#'    | first_period_score_guest | integer | Visiting team goals in that span. |
#'    | second_period_score_home | integer | Home team goals in that span. |
#'    | second_period_score_guest | integer | Visiting team goals in that span. |
#'    | third_period_score_home | integer | Home team goals in that span. |
#'    | third_period_score_guest | integer | Visiting team goals in that span. |
#'    | actual_time_name | character | Display label of the game state in the bucket's language, e.g. `end`, `Ende`, `before game`. |
#'    | actual_time_alias | character | Short game-state code: `K` = finished (Czech `konec`), `0` = not started. |
#'    | last_event_time | integer | Game second of the latest event, of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | arena | character | Arena name; empty string or null where not set. |
#'    | advanced_statistics | logical | True when the game has advanced stats files (shots, boxscore). |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [Metal Ligaen](https://www.metalligaen.dk/) (the league site whose widgets read this public eSports bucket)
#' @keywords Metal Ligaen eSports
#' @family Metal Ligaen eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(metal_schedule(season = 2027, league_id = 1))
#' }
metal_schedule <- function(season, league_id = 1L, return_parsed = TRUE) {
  .esports_schedule("metal", season = season, league_id = league_id, return_parsed = return_parsed)
}

#' @title **Metal Ligaen Top Scorers**
#' @description Skater scoring leaderboard of one competition season.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/league-top-scorers/{season}/{league_id}.json` of the Metal Ligaen eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/league-top-scorers/2026/1.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param league_id Competition id. 1 = Metal Ligaen, regular season and playoffs in one file (split them by league_part_name); 2 = pre-season, 4 = Final-4 cup.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | name | character | Player first name. |
#'    | surname | character | Player last name. |
#'    | birthdate | character | Birth date `YYYY-MM-DD`. |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_name | character | Team display name. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | goals | integer | Goals the player scored in the league-season. |
#'    | assists | integer | Assists credited to the player in the league-season. |
#'    | points | integer | Points, goals + assists. |
#'    | shots | integer | Shots on goal. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [Metal Ligaen](https://www.metalligaen.dk/) (the league site whose widgets read this public eSports bucket)
#' @keywords Metal Ligaen eSports
#' @family Metal Ligaen eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(metal_top_scorers(season = 2027, league_id = 1))
#' }
metal_top_scorers <- function(season, league_id = 1L, return_parsed = TRUE) {
  .esports_top_scorers("metal", season = season, league_id = league_id, return_parsed = return_parsed)
}

#' @title **Metal Ligaen Game Info**
#' @description Game info of one match: state, arena, attendance, officials, coaches, period scores.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/matches/{match_id}/game-header.json` of the Metal Ligaen eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/matches/575/game-header.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in metal_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | actual_time_name | character | Display label of the game state, e.g. `end` / `Ende`. |
#'    | actual_time_alias | character | Short game-state code: `K` = finished. |
#'    | status | character | Match status: `BEFORE_MATCH`, `LIVE`, `AFTER_MATCH`. |
#'    | stadium | character | Name of the arena the game was played in. |
#'    | number_of_viewers | integer | Spectators at the game (attendance). |
#'    | referees_head_referee1 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | referees_head_referee2 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | referees_line_referee1 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | referees_line_referee2 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | referees_head_referee1number | integer | Official's sweater number; null when not recorded. |
#'    | referees_head_referee2number | integer | Official's sweater number; null when not recorded. |
#'    | referees_line_referee1number | integer | Official's sweater number; null when not recorded. |
#'    | referees_line_referee2number | integer | Official's sweater number; null when not recorded. |
#'    | referees_head_referee1id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
#'    | referees_head_referee2id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
#'    | referees_line_referee1id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
#'    | referees_line_referee2id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
#'    | team_info_home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_info_home_name | character | Team display name. |
#'    | team_info_home_shortcut | character | Three-letter team code. |
#'    | team_info_visitor_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_info_visitor_name | character | Team display name. |
#'    | team_info_visitor_shortcut | character | Three-letter team code. |
#'    | trainers_home_head_coach | character | Head coach (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | trainers_visitor_head_coach | character | Head coach (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
#'    | team_bilance_home_win | integer | Wins of that team in the head-to-head record (unverified). |
#'    | team_bilance_visitor_win | integer | Wins of that team in the head-to-head record (unverified). |
#'    | results_extra_time | logical | True when the game went to overtime. |
#'    | results_shooting | logical | True when the game was decided by a shootout. |
#'    | results_contumacy | logical | True when the result was awarded by forfeit. |
#'    | results_score_final_score_home | integer | Home team goals in that span. |
#'    | results_score_final_score_guest | integer | Visiting team goals in that span. |
#'    | results_score_first_period_score_home | integer | Home team goals in that span. |
#'    | results_score_first_period_score_guest | integer | Visiting team goals in that span. |
#'    | results_score_second_period_score_home | integer | Home team goals in that span. |
#'    | results_score_second_period_score_guest | integer | Visiting team goals in that span. |
#'    | results_score_third_period_score_home | integer | Home team goals in that span. |
#'    | results_score_third_period_score_guest | integer | Visiting team goals in that span. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [Metal Ligaen](https://www.metalligaen.dk/) (the league site whose widgets read this public eSports bucket)
#' @keywords Metal Ligaen eSports
#' @family Metal Ligaen eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(metal_game_info(match_id = 575))
#' }
metal_game_info <- function(match_id, return_parsed = TRUE) {
  .esports_game_info("metal", match_id = match_id, return_parsed = return_parsed)
}

#' @title **Metal Ligaen Pbp**
#' @description Play-by-play of one match: goals, penalties, goalie changes and timeouts, one row per event.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/matches/{match_id}/period-events.json` of the Metal Ligaen eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/matches/575/period-events.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in metal_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | event_index | integer | Row order of the event within the game (0-based), across periods. |
#'    | period | character | Period key as the body sends it: 1, 2, 3, OT (overtime) or SO (shootout). |
#'    | period_number | integer | Period number: 1-3, 4 = overtime, 5 = shootout. |
#'    | time | integer | Game second of the event from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | event_type | character | Event type: goal, penalty, goalkeeperChange, timeout, periodStart, periodEnd, videoReview, ... |
#'    | event_id | integer | Event id within its event type. |
#'    | team_side | character | Team of the row: home or visitor. |
#'    | goalie_player_id | integer | Player id (playerId key space) of the goalie entering the game (goalkeeperChange). |
#'    | goalie_jersey | integer | Jersey number of the goalie entering the game (goalkeeperChange). |
#'    | goalie_name | character | Full name of the goalie entering the game (goalkeeperChange). |
#'    | create_time | integer | Game second at which the event was entered (unverified). |
#'    | penalized_player_id | integer | Player id (playerId key space) of the penalized player. |
#'    | penalized_jersey | integer | Jersey number of the penalized player. |
#'    | penalized_name | character | Full name of the penalized player. |
#'    | penalty_from_real_time | character | Penalty start, local wall clock YYYY-MM-DD HH:MM:SS. |
#'    | penalty_from_scoreboard_time | integer | Penalty start, game second. |
#'    | penalty_to_real_time | character | Penalty end, local wall clock YYYY-MM-DD HH:MM:SS. |
#'    | penalty_to_scoreboard_time | integer | Penalty end, game second. |
#'    | duration | integer | Penalty length in seconds (120 = minor). |
#'    | shooting | logical | True for a goal scored on a penalty shot. |
#'    | disciplined_functionary | logical | True when the penalty was assessed to a team official. |
#'    | disciplined_bm | logical | True when the penalty was a bench minor. |
#'    | codename | character | Penalty code, e.g. `HOOK`, `TRIP`, `ROUGH`, `BOARD`, `DELAY`. |
#'    | current_score | character | Score after the goal, as `home:visitor` text, e.g. `1:0`. |
#'    | real_time | character | Wall-clock local time, `YYYY-MM-DD HH:MM:SS`. |
#'    | scoreboard_time | integer | Seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
#'    | balance | character | Goal strength: `EQ` even, `PP1` / `PP2` power play, `SH1` / `SH2` shorthanded. |
#'    | extra_attacker | logical | True when the scoring team had an extra attacker on (goalie pulled). |
#'    | empty_net | logical | True when the goal went into an empty net. |
#'    | technical | logical | True for a technical penalty (unverified). |
#'    | scorer_player_id | integer | Player id (playerId key space) of the goal scorer. |
#'    | scorer_jersey | integer | Jersey number of the goal scorer. |
#'    | scorer_name | character | Full name of the goal scorer. |
#'    | assist1_player_id | integer | Player id (playerId key space) of the first assist. |
#'    | assist1_jersey | integer | Jersey number of the first assist. |
#'    | assist1_name | character | Full name of the first assist. |
#'    | assist2_player_id | integer | Player id (playerId key space) of the second assist. |
#'    | assist2_jersey | integer | Jersey number of the second assist. |
#'    | assist2_name | character | Full name of the second assist. |
#'    | plus_player_ids | character | Comma-separated player ids (playerId key space) of the scoring team's players on ice. |
#'    | minus_player_ids | character | Comma-separated player ids (playerId key space) of the conceding team's players on ice. |
#'    | replaced_player_id | integer | Player id (playerId key space) of the player serving the penalty in the penalized player's place. |
#'    | replaced_jersey | integer | Jersey number of the player serving the penalty in the penalized player's place. |
#'    | replaced_name | character | Full name of the player serving the penalty in the penalized player's place. |
#'    | outgoing_goalie_player_id | integer | Player id (playerId key space) of the goalie leaving the game (goalkeeperChange). |
#'    | outgoing_goalie_jersey | integer | Jersey number of the goalie leaving the game (goalkeeperChange). |
#'    | outgoing_goalie_name | character | Full name of the goalie leaving the game (goalkeeperChange). |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | goal_review | character | Video review subject, e.g. a goal (EIHL `videoReview` events). |
#'    | on_ice_decision | character | Call on the ice before the review. |
#'    | reason_for_review | character | Why the play was reviewed (free text). |
#'    | decision_after_review | character | Final call after the review. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [Metal Ligaen](https://www.metalligaen.dk/) (the league site whose widgets read this public eSports bucket)
#' @keywords Metal Ligaen eSports
#' @family Metal Ligaen eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(metal_pbp(match_id = 575))
#' }
metal_pbp <- function(match_id, return_parsed = TRUE) {
  .esports_pbp("metal", match_id = match_id, return_parsed = return_parsed)
}

#' @title **Metal Ligaen Player Box**
#' @description Player boxscore of one team in one match, one row per player.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/matches/{match_id}/team-stats/{team_id}.json` of the Metal Ligaen eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/matches/575/team-stats/8.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in metal_schedule). Not the league website's game id.
#' @param team_id Team id (per-bucket team key space: home_id / guest_id in metal_schedule).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (playerId key space, per bucket, stable across games and seasons). |
#'    | name | character | Player full name. |
#'    | firstname | character | First name of the player in this row. |
#'    | surname | character | Player last name. |
#'    | birthdate | character | Birth date `YYYY-MM-DD`. |
#'    | active | logical | True when the player is active on the team. |
#'    | position | character | Position code: `GK` goalie, `DE` defence, `FW` / `FO` forward (FO meaning unverified). |
#'    | stick | character | Handedness: `left` or `right`; null when unknown. |
#'    | weight | integer | Weight in kg; `0` placeholder when unknown. |
#'    | height | integer | Height in cm; `0` placeholder when unknown. |
#'    | jersey | integer | Jersey number. |
#'    | birth_country | character | Player's country of birth (name). |
#'    | nationality | character | Nationality, country name. |
#'    | nationality_short | character | Nationality, three-letter code (`CAN`, `GER`; `UK` for Britain). |
#'    | team_shortcut | character | Three-letter team code. |
#'    | games | integer | Games played. |
#'    | goals | integer | goals in total (goals_home + goals_away: the body splits it by home and away games). |
#'    | goals_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | goals_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists | integer | assists in total (assists_home + assists_away: the body splits it by home and away games). |
#'    | assists_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | assists_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points | integer | points in total (points_home + points_away: the body splits it by home and away games). |
#'    | points_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | points_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | penalty_minutes | integer | Penalty minutes assessed to the player (this game, or to date in the season / league-part files). |
#'    | positive | integer | Plus: on ice for goals for at even strength or shorthanded. |
#'    | negative | integer | Minus: on ice for goals against at even strength or on the power play. |
#'    | points_per_game | numeric | Average goals plus assists per game played. |
#'    | shot_efficiency | numeric | Shooting percentage, goals / shots on goal x 100. |
#'    | penalty_minutes_per_game | numeric | Average penalty minutes per game played. |
#'    | pp_goals | integer | Power-play goals. |
#'    | sh_goals | integer | Shorthanded goals. |
#'    | gw_goals | integer | Game-winning goals. |
#'    | shots_attempts | integer | Shot attempts (on goal + missed + blocked). |
#'    | shots_on_goal | integer | shots_on_goal in total (shots_on_goal_home + shots_on_goal_away: the body splits it by home and away games). |
#'    | shots_on_goal_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_on_goal_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
#'    | shots_missed | integer | This player's shot attempts that missed the net. |
#'    | shots_blocked | integer | This player's shot attempts that were blocked. |
#'    | blocked_shots_by_player | integer | Opponent shots this player blocked. |
#'    | faceoffs_win | integer | Face-offs the player won (this game, or to date in the season / league-part files). |
#'    | faceoffs_losses | integer | Face-offs the player lost (this game, or to date in the season / league-part files). |
#'    | faceoffs_count | integer | Face-offs the player took (this game, or to date in the season / league-part files). |
#'    | timeonice | numeric | Time on ice, seconds with decimals (tracking-based; differs from `timeOnIce`). |
#'    | timeonice_eq | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | timeonice_pp | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | timeonice_sh | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
#'    | hits | integer | Body checks delivered. |
#'    | xg | numeric | Expected goals of the player's shots. |
#'    | top5 | logical | True when the player ranks in the team's top five. |
#'    | rank | integer | Rank of the player in the file's default sort (points). |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [Metal Ligaen](https://www.metalligaen.dk/) (the league site whose widgets read this public eSports bucket)
#' @keywords Metal Ligaen eSports
#' @family Metal Ligaen eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(metal_player_box(match_id = 575, team_id = 8))
#' }
metal_player_box <- function(match_id, team_id, return_parsed = TRUE) {
  .esports_player_box("metal", match_id = match_id, team_id = team_id, return_parsed = return_parsed)
}

#' @title **Metal Ligaen Goalie Box**
#' @description Goalie lines of one match, one row per goalie who played.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/matches/{match_id}/top-goalies.json` of the Metal Ligaen eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/matches/575/top-goalies.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in metal_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
#'    | name | character | Goalie first name. |
#'    | surname | character | Goalie last name. |
#'    | photo | character | Goalie photo URL; null when none. |
#'    | birthdate | character | Birth date `YYYY-MM-DD`. |
#'    | team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | team_name | character | Team display name. |
#'    | team_shortcut | character | Three-letter team code. |
#'    | team_type | character | Side: `home` or `visitor`; null in DEL and EIHL. |
#'    | jersey | integer | Jersey number. |
#'    | matches | integer | Games played (1 in a per-game file). |
#'    | won_matches | integer | Games the goalie won. |
#'    | lost_matches | integer | Games the goalie lost. |
#'    | saves | integer | Shots on goal the goalie stopped. |
#'    | goals_against | integer | Goals the goalie allowed. |
#'    | shots_against | integer | Shots on goal faced. |
#'    | saves_percent | numeric | Save percentage x 100 (e.g. `85.71`). |
#'    | average | numeric | Goals-against average per 60 minutes. |
#'    | zeros | integer | Shutouts by the goalie (`zeros`: games with no goal allowed). |
#'    | time_on_ice | integer | Time in goal, seconds. |
#'    | seconds_per_game | numeric | Time in goal per game, seconds. |
#'    | minutes_in_play | numeric | Share of the team's game time in goal, percent (100.0 = the whole game). |
#'    | visible_games | logical | Widget flag: show the milestone for that stat. |
#'    | visible_shutouts | logical | Widget flag: show the milestone for that stat. |
#'    | visible_wins | logical | Widget flag: show the milestone for that stat. |
#'    | stars_games | integer | Widget star marker for that stat (unverified). |
#'    | stars_shutouts | integer | Widget star marker for that stat (unverified). |
#'    | stars_wins | integer | Widget star marker for that stat (unverified). |
#'    | milestone_games | numeric | Widget value for the next milestone of that stat (unverified). |
#'    | milestone_shutouts | numeric | Widget value for the next milestone of that stat (unverified). |
#'    | milestone_wins | numeric | Widget value for the next milestone of that stat (unverified). |
#'    | xg | numeric | Expected goals of the goalie's own team (ICEHL only). |
#'    | xga | numeric | Expected goals against (ICEHL only; null elsewhere). |
#'    | gsax | numeric | Goals saved above expected, xGA minus goals against (ICEHL only). |
#'    | xs | numeric | Expected saves (ICEHL only; unverified). |
#'    | gate_zone_ur_goals_against | integer | Goals allowed in that net zone. |
#'    | gate_zone_ur_shots_against | integer | Shots on goal faced in that net zone. |
#'    | gate_zone_ur_saves | integer | Saves in that net zone. |
#'    | gate_zone_ur_saves_percent | numeric | Save percentage x 100 in that net zone. |
#'    | gate_zone_ul_goals_against | integer | Goals allowed in that net zone. |
#'    | gate_zone_ul_shots_against | integer | Shots on goal faced in that net zone. |
#'    | gate_zone_ul_saves | integer | Saves in that net zone. |
#'    | gate_zone_ul_saves_percent | numeric | Save percentage x 100 in that net zone. |
#'    | gate_zone_br_goals_against | integer | Goals allowed in that net zone. |
#'    | gate_zone_br_shots_against | integer | Shots on goal faced in that net zone. |
#'    | gate_zone_br_saves | integer | Saves in that net zone. |
#'    | gate_zone_br_saves_percent | numeric | Save percentage x 100 in that net zone. |
#'    | gate_zone_fh_goals_against | integer | Goals allowed in that net zone. |
#'    | gate_zone_fh_shots_against | integer | Shots on goal faced in that net zone. |
#'    | gate_zone_fh_saves | integer | Saves in that net zone. |
#'    | gate_zone_fh_saves_percent | numeric | Save percentage x 100 in that net zone. |
#'    | gate_zone_bl_goals_against | integer | Goals allowed in that net zone. |
#'    | gate_zone_bl_shots_against | integer | Shots on goal faced in that net zone. |
#'    | gate_zone_bl_saves | integer | Saves in that net zone. |
#'    | gate_zone_bl_saves_percent | numeric | Save percentage x 100 in that net zone. |
#'    | gate_zone_ce_goals_against | integer | Goals allowed in that net zone. |
#'    | gate_zone_ce_shots_against | integer | Shots on goal faced in that net zone. |
#'    | gate_zone_ce_saves | integer | Saves in that net zone. |
#'    | gate_zone_ce_saves_percent | numeric | Save percentage x 100 in that net zone. |
#'    | rank | integer | Rank among the game's goalies; null in DEL and EIHL. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [Metal Ligaen](https://www.metalligaen.dk/) (the league site whose widgets read this public eSports bucket)
#' @keywords Metal Ligaen eSports
#' @family Metal Ligaen eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(metal_goalie_box(match_id = 575))
#' }
metal_goalie_box <- function(match_id, return_parsed = TRUE) {
  .esports_goalie_box("metal", match_id = match_id, return_parsed = return_parsed)
}

#' @title **Metal Ligaen Standings**
#' @description Standings of one competition season, one row per team.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/table/{season}/{league_id}.json` of the Metal Ligaen eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/table/2026/1.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param season Season by its END year (2027 = 2026/27). The bucket keys seasons by their START year; the wrapper requests season - 1.
#' @param league_id Competition id. 1 = Metal Ligaen, regular season and playoffs in one file (split them by league_part_name); 2 = pre-season, 4 = Final-4 cup.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | group_id | integer | Group id (group key space); null when the stage has no groups, so unobserved here. |
#'    | group_name | character | Group name; empty string when the stage has no groups. |
#'    | complete | logical | True when the standings include every finished game. |
#'    | team_id | integer | Team id (per-bucket team key space); joins to home_id / guest_id in the schedule. |
#'    | part_team_id | integer | Team-in-league-part id (its own key space: one team in one league part); no other captured file carries it. |
#'    | name | character | Team display name. |
#'    | team_original | character | Team's registered name. |
#'    | shortcut | character | Three-letter team code. |
#'    | position | integer | Table position, 1 = first. |
#'    | points | integer | Table points. |
#'    | games | integer | Games played. |
#'    | wins | integer | Wins in regulation (DEL, EIHL, Denmark) or all wins (ICEHL, see `winsReg`); definition per bucket unverified. |
#'    | wins_ot | integer | Wins after overtime or shootout. |
#'    | wins_reg | integer | Wins in regulation time. |
#'    | ties | integer | Ties (0 under current rules). |
#'    | losts_ot | integer | Losses after overtime or shootout. |
#'    | losts | integer | Losses in regulation time. |
#'    | score1 | integer | Goals the team scored (goals for). |
#'    | score2 | integer | Goals against. |
#'    | goals_away | integer | Goals scored in away games. |
#'    | points_away | integer | Table points from away games. |
#'    | last10 | character | Form over the last ten games as a result string; empty string in the samples. |
#'    | only_ot_wins | integer | Wins in overtime. |
#'    | only_so_wins | integer | Wins in a shootout. |
#'    | only_ot_losts | integer | Losses in overtime. |
#'    | only_so_losts | integer | Losses in a shootout. |
#'    | qualified | logical | True when the team has clinched qualification. |
#'    | live | logical | True while the team is playing a live game. |
#'    | live_game_score | character | Score of the live game; null when none, so unobserved here. |
#'    | position_change | integer | Change in table position since the previous round. |
#'    | ppg | numeric | Points per game. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [Metal Ligaen](https://www.metalligaen.dk/) (the league site whose widgets read this public eSports bucket)
#' @keywords Metal Ligaen eSports
#' @family Metal Ligaen eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(metal_standings(season = 2027, league_id = 1))
#' }
metal_standings <- function(season, league_id = 1L, return_parsed = TRUE) {
  .esports_standings("metal", season = season, league_id = league_id, return_parsed = return_parsed)
}

#' @title **Metal Ligaen Game Center**
#' @description Game-center bundle of one match as one row: game data, team stats, goal and penalty lists.
#'
#'   Wraps `GET https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/widget/esports/match/{match_id}.json` of the Metal Ligaen eSports bucket. Example: <https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/widget/esports/match/575.json>.
#'   Seasons are END years (2027 = 2026/27); every request waits at least one second after
#'   the last one to the same host.
#' @param match_id Match id (per-bucket match key space: match_id in metal_schedule). Not the league website's game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw decoded JSON body (a nested list).
#' @return A [tibble::tibble()] with one row per row of the file (zero rows, carrying the documented columns, for an empty frame); the raw JSON list when `return_parsed = FALSE`. Columns:
#'
#'    | col_name | type | description |
#'    |:---------|:-----|:------------|
#'    | status_id | integer | Widget API status code, a code and not a joinable id: `1` = OK. |
#'    | status_msg | character | Widget API status text, `Ok`. |
#'    | data_game_data_id | integer | Match id (match key space, per bucket); joins to `league-matches` `matches[].id` and the `{match_id}` path param. |
#'    | data_game_data_division_id | integer | League part id under the widget's name `divisionId` (league-part key space, per bucket: one stage of a competition, e.g. ICEHL 56 = 2026/27 regular season); joins to `league_part_id` in league-matches and the `{league_part_id}` path param. |
#'    | data_game_data_division_longname | character | League part name, e.g. `ICEHL 2026/27`. |
#'    | data_game_data_division_shortname | character | League part name, e.g. `ICEHL 2026/27`. |
#'    | data_game_data_scheduled_date_formatted_short | character | Short display text `DD.MM.YY` (plus `HH:MM` for times). |
#'    | data_game_data_scheduled_date_formatted_long | character | Long display text, e.g. `18.September. 2026`. |
#'    | data_game_data_scheduled_date_timestamp | numeric | Epoch milliseconds. |
#'    | data_game_data_scheduled_date_value | character | ISO text `YYYY-MM-DD` (plus ` HH:MM` for times). |
#'    | data_game_data_scheduled_date_diff_to_now | integer | Difference to now as computed by the widget server; `0` in the samples. |
#'    | data_game_data_scheduled_time | character | Local time `HH:MM` (scheduled face-off, actual start, actual end). |
#'    | data_game_data_start_time | character | Local time `HH:MM` (scheduled face-off, actual start, actual end). |
#'    | data_game_data_end_time | character | Local time `HH:MM` (scheduled face-off, actual start, actual end). |
#'    | data_game_data_is_overtime | integer | Overtime flag sent as an integer, `1` = yes. |
#'    | data_game_data_is_shoot_out | integer | Shootout flag sent as an integer, `1` = yes. |
#'    | data_game_data_home_team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | data_game_data_home_team_longname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
#'    | data_game_data_home_team_shortname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
#'    | data_game_data_home_team_flavourname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
#'    | data_game_data_home_team_score | integer | Final goals of the home or away team (by name). |
#'    | data_game_data_away_team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
#'    | data_game_data_away_team_longname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
#'    | data_game_data_away_team_shortname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
#'    | data_game_data_away_team_flavourname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
#'    | data_game_data_away_team_score | integer | Final goals of the home or away team (by name). |
#'    | data_game_data_game_status | integer | Game status code; `4` = finished in the samples (other codes unverified). |
#'    | data_game_data_game_extended_status | integer | Extended status code; `0` in the samples. |
#'    | data_game_data_game_extended_status_info_text | character | Extended status text; empty string in the samples. |
#'    | data_game_data_game_name | character | Game number sent as a string (e.g. `"66"`). |
#'    | data_game_data_game_day | character | Matchday number sent as a string (`"1"`); null in Denmark. |
#'    | data_game_data_game_round | integer | Round number; `0` in the samples. |
#'    | data_game_data_location_longname | character | Arena name; empty string when not set. |
#'    | data_game_data_location_shortname | character | Arena name; empty string when not set. |
#'    | data_game_data_location_coordinates_lat | integer | Latitude / longitude; `0` placeholder in the samples. |
#'    | data_game_data_location_coordinates_lng | integer | Latitude / longitude; `0` placeholder in the samples. |
#'    | data_game_data_attendance | integer | Spectators at the game (attendance). |
#'    | data_game_data_live_time | integer | Game clock, seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). 3600 at the end of regulation. |
#'    | data_game_data_live_time_formatted | character | Clock text; the placeholder `END-GAME` after the game, `m:ss` while live. |
#'    | data_game_data_live_time_period | integer | Current period number; `4` after the game. |
#'    | data_game_data_live_time_string | character | Clock text; the placeholder `END-GAME` after the game, `m:ss` while live. |
#'    | data_game_data_live_time_game_phase | character | Game phase text, e.g. `END OF GAME`. |
#'    | data_game_data_period_stats | character | Per-period score and shots. |
#'    | data_game_data_game_officials_ref1_lastname | character | Last name of that official or coach. |
#'    | data_game_data_game_officials_ref1_firstname | character | First name of that official or coach. |
#'    | data_game_data_game_officials_ref2_lastname | character | Last name of that official or coach. |
#'    | data_game_data_game_officials_ref2_firstname | character | First name of that official or coach. |
#'    | data_game_data_game_officials_linesman1_lastname | character | Last name of that official or coach. |
#'    | data_game_data_game_officials_linesman1_firstname | character | First name of that official or coach. |
#'    | data_game_data_game_officials_linesman2_lastname | character | Last name of that official or coach. |
#'    | data_game_data_game_officials_linesman2_firstname | character | First name of that official or coach. |
#'    | data_game_data_team_officials_headcoachhome_lastname | character | Last name of that official or coach. |
#'    | data_game_data_team_officials_headcoachhome_firstname | character | First name of that official or coach. |
#'    | data_game_data_team_officials_headcoachaway_lastname | character | Last name of that official or coach. |
#'    | data_game_data_team_officials_headcoachaway_firstname | character | First name of that official or coach. |
#'    | data_game_data_live_time_period_bar | character | Period progress bar; null in the samples, so unobserved. |
#'    | data_game_data_you_tube_link | character | Video link; null in the samples, so unobserved. |
#'    | data_game_data_number_of_periods | integer | Regulation periods (3) or the maximum overtimes allowed. |
#'    | data_game_data_length_of_period | integer | Length of a period or overtime, seconds (1200). |
#'    | data_game_data_number_of_overtimes | integer | Regulation periods (3) or the maximum overtimes allowed. |
#'    | data_game_data_length_of_overtime | integer | Length of a period or overtime, seconds (1200). |
#'    | data_game_data_shootout_shots | integer | Shootout attempts in the game. |
#'    | data_game_data_images_home_team_logo | character | Team logo; null in the samples, so unobserved. |
#'    | data_game_data_images_away_team_logo | character | Team logo; null in the samples, so unobserved. |
#'    | data_game_data_streams | character | Video streams; empty array in the samples. |
#'    | data_game_data_home_team_logo_url | character | Team logo URL; null in the samples, so unobserved. |
#'    | data_game_data_away_team_logo_url | character | Team logo URL; null in the samples, so unobserved. |
#'    | data_home_goals | character | Goals scored by that team. |
#'    | data_home_penalties | character | Penalties of that team. |
#'    | data_home_field_players | character | Skaters of that team with their game stats. |
#'    | data_home_goal_keepers | character | Goalies of that team with their game stats. |
#'    | data_home_team_stats_faceoffs_won | integer | Face-offs the team won in this game. |
#'    | data_home_team_stats_faceoffs_total | integer | Face-offs the team took in this game. |
#'    | data_home_team_stats_shots_on_goal | integer | Team shots that reached the goalie in this game. |
#'    | data_home_team_stats_shots_blocked | integer | Team blocked-shot count in this game (direction unverified). |
#'    | data_home_team_stats_penalty_minutes | integer | Penalty minutes assessed to the team in this game. |
#'    | data_home_team_stats_powerplay_goals | integer | Goals the team scored on the power play in this game. |
#'    | data_home_team_stats_powerplay_opportunities | integer | Power plays the team had in this game. |
#'    | data_home_team_stats_penaltykill_successes | integer | Penalties killed without conceding. |
#'    | data_home_team_stats_penaltykill_situations | integer | Times the team had to kill a penalty in this game. |
#'    | data_home_team_stats_time_values_leading | integer | Seconds spent leading, tied or trailing (by name). |
#'    | data_home_team_stats_time_values_tied | integer | Seconds spent leading, tied or trailing (by name). |
#'    | data_home_team_stats_time_values_trailing | integer | Seconds spent leading, tied or trailing (by name). |
#'    | data_home_team_stats_time_values_time_game_strength_even | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_home_team_stats_time_values_time_game_strength_plus_one | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_home_team_stats_time_values_time_game_strength_plus_two | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_home_team_stats_time_values_time_game_strength_minus_one | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_home_team_stats_time_values_time_game_strength_minus_two | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_home_team_stats_time_values_time_on_powerplay | integer | Seconds on the power play. |
#'    | data_home_team_stats_time_values_time_while_shorthanded | integer | Seconds the team played shorthanded in this game. |
#'    | data_away_goals | character | Goals scored by that team. |
#'    | data_away_penalties | character | Penalties of that team. |
#'    | data_away_field_players | character | Skaters of that team with their game stats. |
#'    | data_away_goal_keepers | character | Goalies of that team with their game stats. |
#'    | data_away_team_stats_faceoffs_won | integer | Face-offs the team won in this game. |
#'    | data_away_team_stats_faceoffs_total | integer | Face-offs the team took in this game. |
#'    | data_away_team_stats_shots_on_goal | integer | Team shots that reached the goalie in this game. |
#'    | data_away_team_stats_shots_blocked | integer | Team blocked-shot count in this game (direction unverified). |
#'    | data_away_team_stats_penalty_minutes | integer | Penalty minutes assessed to the team in this game. |
#'    | data_away_team_stats_powerplay_goals | integer | Goals the team scored on the power play in this game. |
#'    | data_away_team_stats_powerplay_opportunities | integer | Power plays the team had in this game. |
#'    | data_away_team_stats_penaltykill_successes | integer | Penalties killed without conceding. |
#'    | data_away_team_stats_penaltykill_situations | integer | Times the team had to kill a penalty in this game. |
#'    | data_away_team_stats_time_values_leading | integer | Seconds spent leading, tied or trailing (by name). |
#'    | data_away_team_stats_time_values_tied | integer | Seconds spent leading, tied or trailing (by name). |
#'    | data_away_team_stats_time_values_trailing | integer | Seconds spent leading, tied or trailing (by name). |
#'    | data_away_team_stats_time_values_time_game_strength_even | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_away_team_stats_time_values_time_game_strength_plus_one | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_away_team_stats_time_values_time_game_strength_plus_two | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_away_team_stats_time_values_time_game_strength_minus_one | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_away_team_stats_time_values_time_game_strength_minus_two | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
#'    | data_away_team_stats_time_values_time_on_powerplay | integer | Seconds on the power play. |
#'    | data_away_team_stats_time_values_time_while_shorthanded | integer | Seconds the team played shorthanded in this game. |
#'    | data_home_goal_keeper_changes | character | Goalie on/off events of that team. |
#'    | data_away_goal_keeper_changes | character | Goalie on/off events of that team. |
#'    | data_game_actions | character | Empty array in both samples; item shape unobserved. |
#'    | data_faceoffs_head_to_head | character | Empty array in both samples; item shape unobserved. |
#'    | data_faceoffs_individual | character | Empty array in both samples; item shape unobserved. |
#'    | data_game_situations | character | Empty array in both samples; item shape unobserved. |
#'    | calc_time | integer | Server computation time; `0` in the samples. |
#'    | last_update_formatted_short | character | Short display text `DD.MM.YY` (plus `HH:MM` for times). |
#'    | last_update_formatted_long | character | Long display text, e.g. `18.September. 2026`. |
#'    | last_update_timestamp | numeric | Epoch milliseconds. |
#'    | last_update_value | character | ISO text `YYYY-MM-DD` (plus ` HH:MM` for times). |
#'    | served_by_server | character | Host that built the bundle, e.g. `icehl.hokejovyzapis.cz`. |
#'
#' @section Errors:
#' Signalled as classed conditions: `fastRhockey_no_data` when the bucket has no such key (S3 404 NoSuchKey: unknown season, competition, match or team) or the file holds no rows (an unplayed game, an empty shot shell); `fastRhockey_asset_fetch` for any other non-2xx answer (403 AccessDenied), a connection failure after retries, an empty or unreadable 200 body, an error envelope, a body of the wrong shape, or a body for another season or game; `fastRhockey_bad_request` for an HTTP 400/422 or an invalid argument. All three also inherit `fastRhockey_error`. Wrappers raise; they never warn and return an empty frame.
#'
#' @seealso [Metal Ligaen](https://www.metalligaen.dk/) (the league site whose widgets read this public eSports bucket)
#' @keywords Metal Ligaen eSports
#' @family Metal Ligaen eSports functions
#' @export
#' @examples
#' \donttest{
#'   try(metal_game_center(match_id = 575))
#' }
metal_game_center <- function(match_id, return_parsed = TRUE) {
  .esports_game_center("metal", match_id = match_id, return_parsed = return_parsed)
}
