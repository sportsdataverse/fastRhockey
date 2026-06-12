# **NHL Records - Goalie Career Stats**

Returns career goalie statistics from the NHL Records API
(`https://records.nhl.com/site/api/goalie-career-stats`).

## Usage

``` r
nhl_records_goalie_career_stats(cayenne_exp = NULL, limit = NULL, start = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string (e.g. `"playerId=8471679"`).

- limit:

  Optional integer page size.

- start:

  Optional integer pagination offset.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| id | integer | Unique record identifier. |
| active_player | logical | Indicator of whether the player is active. |
| first_name | character | Player first name. |
| first_season_for_game_type | integer | First season ID for the game type. |
| franchise_id | integer | Unique franchise identifier. |
| game_seven_games_played | logical | Game seven games played. |
| game_seven_losses | logical | Game seven losses. |
| game_seven_wins | logical | Game seven wins. |
| game_type_id | integer | Game type the totals belong to. |
| games_played | integer | Total games played. |
| goals_against | integer | Goals against. |
| goals_against_average | numeric | Goals against average. |
| last_name | character | Player last name. |
| last_season_for_game_type | integer | Last season ID for the game type. |
| losses | integer | Total losses. |
| overtime_games_played | integer | Overtime games played. |
| overtime_goals_against | integer | Overtime goals against. |
| overtime_goals_against_average | logical | Overtime goals against average. |
| overtime_losses | logical | Overtime losses. |
| overtime_save_pctg | numeric | Overtime save percentage. |
| overtime_shots_against | integer | Overtime shots against. |
| overtime_ties | integer | Overtime ties. |
| overtime_time_on_ice | integer | Overtime time on ice (seconds). |
| overtime_wins | integer | Overtime wins. |
| player_id | integer | Unique player identifier. |
| position_code | character | Player position code. |
| save_pctg | numeric | Save percentage. |
| saves | integer | Saves made. |
| seasons_played | integer | Number of seasons played. |
| shots_against | integer | Shots faced. |
| shutouts | integer | Shutouts recorded. |
| team_abbrevs | character | Team abbreviations. |
| team_names | character | Team names. |
| ties | integer | Total ties. |
| time_on_ice | integer | Total time on ice (seconds). |
| time_on_ice_min_sec | character | Total time on ice (MM:SS). |
| wins | integer | Total wins. |

## Examples

``` r
# \donttest{
  try(nhl_records_goalie_career_stats(limit = 5))
#> ── NHL Records Goalie Career Stats ──────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 13:22:29 UTC
#> # A tibble: 5 × 37
#>      id active_player first_name first_season_for_game_type franchise_id
#>   <int> <lgl>         <chr>                           <int>        <int>
#> 1   151 FALSE         Hardy                        19771978           NA
#> 2  1421 FALSE         Hardy                        19771978           10
#> 3  2108 FALSE         Hardy                        19791980           23
#> 4   155 FALSE         Steve                        19791980           NA
#> 5  1443 FALSE         Steve                        19791980           10
#> # ℹ 32 more variables: game_seven_games_played <lgl>, game_seven_losses <lgl>,
#> #   game_seven_wins <lgl>, game_type_id <int>, games_played <int>,
#> #   goals_against <int>, goals_against_average <dbl>, last_name <chr>,
#> #   last_season_for_game_type <int>, losses <int>, overtime_games_played <int>,
#> #   overtime_goals_against <int>, overtime_goals_against_average <lgl>,
#> #   overtime_losses <lgl>, overtime_save_pctg <dbl>,
#> #   overtime_shots_against <int>, overtime_ties <int>, …
# }
```
