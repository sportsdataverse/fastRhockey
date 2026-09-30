# **NHL Teams Stats**

Returns NHL team player-level stats (skaters and goalies) for a given
team abbreviation and season. Uses the new NHL API club-stats endpoint
(`api-web.nhle.com`).

**Breaking change:** The old `team_id` (integer) parameter has been
replaced by `team_abbr` (3-letter string). The `season` parameter now
accepts a 4-digit year (e.g., 2024 for the 2024-25 season).

## Usage

``` r
nhl_teams_stats(team_abbr, season = NULL, game_type = 2)
```

## Arguments

- team_abbr:

  Three-letter team abbreviation (e.g., "TBL", "TOR", "SEA")

- season:

  Integer 4-digit year (e.g., 2024 for the 2024-25 season). If NULL,
  returns current season stats.

- game_type:

  Integer game type: 2 = regular season (default), 3 = playoffs

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| player_id | integer | Unique player identifier. |
| headshot | character | URL to the player's headshot image. |
| position_code | character | Player position code. |
| games_played | integer | Games played. |
| goals | integer | Goals scored. |
| assists | integer | Assists. |
| points | integer | Total points (goals + assists). |
| plus_minus | integer | Plus/minus rating. |
| penalty_minutes | integer | Penalty minutes. |
| power_play_goals | integer | Power-play goals. |
| shorthanded_goals | integer | Short-handed goals. |
| game_winning_goals | integer | Game-winning goals. |
| overtime_goals | integer | Overtime goals. |
| shots | integer | Shots on goal. |
| shooting_pctg | numeric | Shooting percentage. |
| avg_time_on_ice_per_game | numeric | Average time on ice per game. |
| avg_shifts_per_game | numeric | Average shifts per game. |
| faceoff_win_pctg | numeric | Faceoff win percentage. |
| first_name_default | character | Player first name (default language). |
| last_name_default | character | Player last name (default language). |
| last_name_cs | character | Player last name (Czech localization). |
| last_name_fi | character | Player last name (Finnish localization). |
| last_name_sk | character | Player last name (Slovak localization). |
| player_type | character | Player type ("skater" or "goalie"). |
| games_started | integer | Games started (goalies). |
| wins | integer | Wins (goalies). |
| losses | integer | Losses (goalies). |
| overtime_losses | integer | Overtime losses (goalies). |
| goals_against_average | numeric | Goals-against average (goalies). |
| save_percentage | numeric | Save percentage (goalies). |
| shots_against | integer | Shots faced (goalies). |
| saves | integer | Saves made (goalies). |
| goals_against | integer | Goals against (goalies). |
| shutouts | integer | Shutouts (goalies). |
| time_on_ice | integer | Total time on ice (goalies). |
| first_name_cs | character | Player first name (Czech localization). |
| first_name_sk | character | Player first name (Slovak localization). |
| team_abbr | character | Team abbreviation. |
| season | character | Season identifier. |
| game_type | integer | Game type (2 = regular season, 3 = playoffs). |

## Examples

``` r
# \donttest{
  try(nhl_teams_stats(team_abbr = "TBL"))
#> 2026-09-30 14:45:34.970262: No stats found for TBL
#> NULL
# }
```
