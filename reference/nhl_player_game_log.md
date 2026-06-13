# **NHL Player Game Log**

Returns game-by-game stats for an NHL player. Supports both current
season and historical season lookups.

## Usage

``` r
nhl_player_game_log(player_id, season = NULL, game_type = 2)
```

## Arguments

- player_id:

  Integer player ID (e.g., 8478402 for Connor McDavid)

- season:

  Integer 4-digit year (e.g., 2024 for the 2024-25 season). If NULL,
  returns the current season game log.

- game_type:

  Integer game type: 2 = regular season (default), 3 = playoffs

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                              |           |                                 |
|------------------------------|-----------|---------------------------------|
| col_name                     | types     | description                     |
| game_id                      | integer   | Unique game identifier.         |
| team_abbrev                  | character | Player's team abbreviation.     |
| home_road_flag               | character | Home or road indicator.         |
| game_date                    | character | Game date.                      |
| goals                        | integer   | Goals scored.                   |
| assists                      | integer   | Assists.                        |
| points                       | integer   | Total points (goals + assists). |
| plus_minus                   | integer   | Plus/minus rating.              |
| power_play_goals             | integer   | Power play goals.               |
| power_play_points            | integer   | Power play points.              |
| game_winning_goals           | integer   | Game-winning goals.             |
| ot_goals                     | integer   | Overtime goals.                 |
| shots                        | integer   | Shots on goal.                  |
| shifts                       | integer   | Number of shifts.               |
| shorthanded_goals            | integer   | Shorthanded goals.              |
| shorthanded_points           | integer   | Shorthanded points.             |
| pim                          | integer   | Penalty minutes.                |
| toi                          | character | Time on ice.                    |
| opponent_abbrev              | character | Opponent team abbreviation.     |
| common_name_default          | character | Player's team common name.      |
| opponent_common_name_default | character | Opponent team common name.      |
| player_id                    | numeric   | Unique player identifier.       |

## Examples

``` r
# \donttest{
  try(nhl_player_game_log(player_id = 8478402))
#> ── NHL Player Game Log ──────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-13 03:25:25 UTC
#> # A tibble: 6 × 22
#>     game_id team_abbrev home_road_flag game_date goals assists points plus_minus
#>       <int> <chr>       <chr>          <chr>     <int>   <int>  <int>      <int>
#> 1    2.03e9 EDM         R              2026-04-…     0       0      0         -3
#> 2    2.03e9 EDM         H              2026-04-…     0       2      2          1
#> 3    2.03e9 EDM         R              2026-04-…     0       2      2          0
#> 4    2.03e9 EDM         R              2026-04-…     1       1      2         -4
#> 5    2.03e9 EDM         H              2026-04-…     0       0      0         -2
#> 6    2.03e9 EDM         H              2026-04-…     0       0      0          0
#> # ℹ 14 more variables: power_play_goals <int>, power_play_points <int>,
#> #   game_winning_goals <int>, ot_goals <int>, shots <int>, shifts <int>,
#> #   shorthanded_goals <int>, shorthanded_points <int>, pim <int>, toi <chr>,
#> #   opponent_abbrev <chr>, common_name_default <chr>,
#> #   opponent_common_name_default <chr>, player_id <dbl>
# }
```
