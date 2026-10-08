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
#> ── NHL Teams Stats Information from NHL.com ─────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-08 11:32:48 UTC
#> # A tibble: 21 × 41
#>    player_id headshot position_code games_played goals assists points plus_minus
#>        <int> <chr>    <chr>                <int> <int>   <int>  <int>      <int>
#>  1   8474151 https:/… D                        3     0       0      0          1
#>  2   8474590 https:/… D                        3     0       2      2          0
#>  3   8475167 https:/… D                        3     0       0      0          1
#>  4   8476453 https:/… R                        3     1       3      4          2
#>  5   8476878 https:/… C                        3     0       1      1          0
#>  6   8477404 https:/… C                        3     1       2      3          2
#>  7   8478010 https:/… C                        3     1       1      2          2
#>  8   8478416 https:/… D                        3     0       0      0          3
#>  9   8478424 https:/… C                        1     0       0      0         -1
#> 10   8478519 https:/… C                        3     0       0      0          1
#> # ℹ 11 more rows
#> # ℹ 33 more variables: penalty_minutes <int>, power_play_goals <int>,
#> #   shorthanded_goals <int>, game_winning_goals <int>, overtime_goals <int>,
#> #   shots <int>, shooting_pctg <dbl>, avg_time_on_ice_per_game <dbl>,
#> #   avg_shifts_per_game <dbl>, faceoff_win_pctg <dbl>,
#> #   first_name_default <chr>, last_name_default <chr>, last_name_cs <chr>,
#> #   last_name_fi <chr>, last_name_sk <chr>, first_name_cs <chr>, …
# }
```
