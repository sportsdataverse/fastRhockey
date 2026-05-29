#' @name nhl_edge
#' @aliases nhl_edge nhl_edge_skater nhl_edge_goalie nhl_edge_team nhl_cat_edge
#' @title **NHL Edge Analytics Endpoint Overview**
#' @description
#' Wrappers around the NHL Edge analytics backend at
#' `api-web.nhle.com/v1/edge/...` (and the `cat` variants at
#' `api-web.nhle.com/v1/cat/edge/...`). These surface the per-skater,
#' per-goalie, and per-team advanced metrics that power the public NHL Edge
#' UI: shot location, shot speed, skating speed, skating distance, zone
#' time, comparisons, and top-10 leaderboards. All wrappers route through
#' `.nhl_edge_api()` / `.nhl_edge_to_df()` in `helpers_nhl_edge.R` so the
#' `/now` vs `/{season}/{game_type}` URL split and the assorted response
#' shapes are handled in one place.
#'
#' @details
#'
#' ## **Skater Edge**
#'
#' | Function | Purpose |
#' |---|---|
#' | [nhl_edge_skater_detail()]                    | Per-skater Edge summary |
#' | [nhl_edge_skater_landing()]                   | Edge landing page payload |
#' | [nhl_edge_skater_comparison()]                | Two-skater Edge comparison |
#' | [nhl_edge_skater_shot_location_detail()]      | Shot location heatmap |
#' | [nhl_edge_skater_shot_location_top_10()]      | Shot-location top-10 |
#' | [nhl_edge_skater_shot_speed_detail()]         | Shot-speed splits |
#' | [nhl_edge_skater_shot_speed_top_10()]         | Shot-speed top-10 |
#' | [nhl_edge_skater_skating_speed_detail()]      | Skating-speed splits |
#' | [nhl_edge_skater_speed_top_10()]              | Skating-speed top-10 |
#' | [nhl_edge_skater_skating_distance_detail()]   | Skating-distance splits |
#' | [nhl_edge_skater_distance_top_10()]           | Skating-distance top-10 |
#' | [nhl_edge_skater_zone_time()]                 | Zone-time splits |
#' | [nhl_edge_skater_zone_time_top_10()]          | Zone-time top-10 |
#' | [nhl_cat_edge_skater_detail()]                | CAT-framework skater detail |
#'
#' ## **Goalie Edge**
#'
#' | Function | Purpose |
#' |---|---|
#' | [nhl_edge_goalie_detail()]                    | Per-goalie Edge summary |
#' | [nhl_edge_goalie_landing()]                   | Edge landing page payload |
#' | [nhl_edge_goalie_comparison()]                | Two-goalie comparison |
#' | [nhl_edge_goalie_5v5_detail()]                | 5-on-5 splits |
#' | [nhl_edge_goalie_5v5_top_10()]                | 5-on-5 top-10 |
#' | [nhl_edge_goalie_save_percentage_detail()]    | Save-percentage splits |
#' | [nhl_edge_goalie_edge_save_pctg_top_10()]     | Save-percentage top-10 |
#' | [nhl_edge_goalie_shot_location_detail()]      | Shot-location heatmap |
#' | [nhl_edge_goalie_shot_location_top_10()]      | Shot-location top-10 |
#' | [nhl_cat_edge_goalie_detail()]                | CAT-framework goalie detail |
#'
#' ## **Team Edge**
#'
#' | Function | Purpose |
#' |---|---|
#' | [nhl_edge_team_detail()]                       | Per-team Edge summary |
#' | [nhl_edge_team_landing()]                      | Edge landing page payload |
#' | [nhl_edge_team_shot_location_detail()]         | Team shot location |
#' | [nhl_edge_team_shot_location_top_10()]         | Team shot location top-10 |
#' | [nhl_edge_team_shot_speed_detail()]            | Team shot speed |
#' | [nhl_edge_team_skating_speed_detail()]         | Team skating speed |
#' | [nhl_edge_team_skating_speed_top_10()]         | Team skating speed top-10 |
#' | [nhl_edge_team_skating_distance_detail()]      | Team skating distance |
#' | [nhl_edge_team_skating_distance_top_10()]      | Team skating distance top-10 |
#' | [nhl_edge_team_zone_time_details()]            | Team zone time |
#' | [nhl_edge_team_zone_time_top_10()]             | Team zone time top-10 |
#'
#' @keywords NHL Edge Analytics
#' @family NHL Edge
NULL
