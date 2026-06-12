# **NHL Records - Goalie Season Stats**

Returns season-by-season goalie statistics from the NHL Records API
(`https://records.nhl.com/site/api/goalie-season-stats`).

## Usage

``` r
nhl_records_goalie_season_stats(cayenne_exp = NULL, limit = NULL, start = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string.

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
| franchise_id | integer | Unique franchise identifier. |
| game_seven_games_played | logical | Game seven games played. |
| game_seven_losses | logical | Game seven losses. |
| game_seven_wins | logical | Game seven wins. |
| game_type | integer | Game type the totals belong to. |
| games_played | integer | Total games played. |
| games_started | integer | Games started. |
| goals_against | integer | Goals against. |
| goals_against_average | numeric | Goals against average. |
| last_name | character | Player last name. |
| losses | integer | Total losses. |
| number_of_games_in_season | integer | Number of games in the season. |
| overtime_games_played | integer | Overtime games played. |
| overtime_goals_against | integer | Overtime goals against. |
| overtime_losses | logical | Overtime losses. |
| overtime_ties | integer | Overtime ties. |
| overtime_wins | integer | Overtime wins. |
| player_id | integer | Unique player identifier. |
| position_code | character | Player position code. |
| rookie_flag | logical | Indicator of whether the player was a rookie. |
| save_pctg | numeric | Save percentage. |
| saves | integer | Saves made. |
| season_id | integer | Season identifier. |
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
  try(nhl_records_goalie_season_stats(limit = 5))
#> ── NHL Records Goalie Season Stats ──────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 02:22:46 UTC
#> # A tibble: 5 × 34
#>      id active_player first_name franchise_id game_seven_games_played
#>   <int> <lgl>         <chr>             <int> <lgl>                  
#> 1  1485 FALSE         Hardy                NA NA                     
#> 2  7213 FALSE         Hardy                10 NA                     
#> 3  1884 FALSE         Hardy                NA NA                     
#> 4 10187 FALSE         Hardy                23 NA                     
#> 5  1816 FALSE         Hardy                NA NA                     
#> # ℹ 29 more variables: game_seven_losses <lgl>, game_seven_wins <lgl>,
#> #   game_type <int>, games_played <int>, games_started <int>,
#> #   goals_against <int>, goals_against_average <dbl>, last_name <chr>,
#> #   losses <int>, number_of_games_in_season <int>, overtime_games_played <int>,
#> #   overtime_goals_against <int>, overtime_losses <lgl>, overtime_ties <int>,
#> #   overtime_wins <int>, player_id <int>, position_code <chr>,
#> #   rookie_flag <lgl>, save_pctg <dbl>, saves <int>, season_id <int>, …
# }
```
