# **NHL Edge Analytics Endpoint Overview**

Wrappers around the NHL Edge analytics backend at
`api-web.nhle.com/v1/edge/...` (and the `cat` variants at
`api-web.nhle.com/v1/cat/edge/...`). These surface the per-skater,
per-goalie, and per-team advanced metrics that power the public NHL Edge
UI: shot location, shot speed, skating speed, skating distance, zone
time, comparisons, and top-10 leaderboards. All wrappers route through
`.nhl_edge_api()` / `.nhl_edge_to_df()` in `helpers_nhl_edge.R` so the
`/now` vs `/{season}/{game_type}` URL split and the assorted response
shapes are handled in one place.

## Details

### **Skater Edge**

|  |  |
|----|----|
| Function | Purpose |
| [`nhl_edge_skater_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_detail.md) | Per-skater Edge summary |
| [`nhl_edge_skater_landing()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_landing.md) | Edge landing page payload |
| [`nhl_edge_skater_comparison()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_comparison.md) | Two-skater Edge comparison |
| [`nhl_edge_skater_shot_location_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_shot_location_detail.md) | Shot location heatmap |
| [`nhl_edge_skater_shot_location_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_shot_location_top_10.md) | Shot-location top-10 |
| [`nhl_edge_skater_shot_speed_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_shot_speed_detail.md) | Shot-speed splits |
| [`nhl_edge_skater_shot_speed_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_shot_speed_top_10.md) | Shot-speed top-10 |
| [`nhl_edge_skater_skating_speed_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_skating_speed_detail.md) | Skating-speed splits |
| [`nhl_edge_skater_speed_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_speed_top_10.md) | Skating-speed top-10 |
| [`nhl_edge_skater_skating_distance_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_skating_distance_detail.md) | Skating-distance splits |
| [`nhl_edge_skater_distance_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_distance_top_10.md) | Skating-distance top-10 |
| [`nhl_edge_skater_zone_time()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_zone_time.md) | Zone-time splits |
| [`nhl_edge_skater_zone_time_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_zone_time_top_10.md) | Zone-time top-10 |
| [`nhl_cat_edge_skater_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_cat_edge_skater_detail.md) | CAT-framework skater detail |

### **Goalie Edge**

|  |  |
|----|----|
| Function | Purpose |
| [`nhl_edge_goalie_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_detail.md) | Per-goalie Edge summary |
| [`nhl_edge_goalie_landing()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_landing.md) | Edge landing page payload |
| [`nhl_edge_goalie_comparison()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_comparison.md) | Two-goalie comparison |
| [`nhl_edge_goalie_5v5_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_5v5_detail.md) | 5-on-5 splits |
| [`nhl_edge_goalie_5v5_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_5v5_top_10.md) | 5-on-5 top-10 |
| [`nhl_edge_goalie_save_percentage_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_save_percentage_detail.md) | Save-percentage splits |
| [`nhl_edge_goalie_edge_save_pctg_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_edge_save_pctg_top_10.md) | Save-percentage top-10 |
| [`nhl_edge_goalie_shot_location_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_shot_location_detail.md) | Shot-location heatmap |
| [`nhl_edge_goalie_shot_location_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_shot_location_top_10.md) | Shot-location top-10 |
| [`nhl_cat_edge_goalie_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_cat_edge_goalie_detail.md) | CAT-framework goalie detail |

### **Team Edge**

|  |  |
|----|----|
| Function | Purpose |
| [`nhl_edge_team_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_detail.md) | Per-team Edge summary |
| [`nhl_edge_team_landing()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_landing.md) | Edge landing page payload |
| [`nhl_edge_team_shot_location_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_shot_location_detail.md) | Team shot location |
| [`nhl_edge_team_shot_location_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_shot_location_top_10.md) | Team shot location top-10 |
| [`nhl_edge_team_shot_speed_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_shot_speed_detail.md) | Team shot speed |
| [`nhl_edge_team_skating_speed_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_skating_speed_detail.md) | Team skating speed |
| [`nhl_edge_team_skating_speed_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_skating_speed_top_10.md) | Team skating speed top-10 |
| [`nhl_edge_team_skating_distance_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_skating_distance_detail.md) | Team skating distance |
| [`nhl_edge_team_skating_distance_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_skating_distance_top_10.md) | Team skating distance top-10 |
| [`nhl_edge_team_zone_time_details()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_zone_time_details.md) | Team zone time |
| [`nhl_edge_team_zone_time_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_zone_time_top_10.md) | Team zone time top-10 |
