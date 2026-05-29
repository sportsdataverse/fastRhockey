# **NHL Skater Summary (Season Range)**

Aggregator helper that calls
[`nhl_stats_skaters()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_skaters.md)
with `report_type = "summary"` for every season in
`[start_season, end_season]` and concatenates the results into a single
tidy frame. Mirrors the `Stats.skater_stats_summary` convenience helper
from the `nhl-api-py` Python client.

## Usage

``` r
nhl_skater_summary_range(start_season, end_season, game_type = 2, limit = 50)
```

## Arguments

- start_season:

  Integer four-digit *end year* of the first season (e.g. `2022` for the
  2021-22 season).

- end_season:

  Integer four-digit *end year* of the final season (inclusive). Must be
  `>= start_season`.

- game_type:

  Integer game type: `2` = regular season (default), `3` = playoffs.

- limit:

  Integer maximum number of rows per season request. Defaults to `50` to
  match
  [`nhl_stats_skaters()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_skaters.md).

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| assists | integer | Assists. |
| ev_goals | integer | Even-strength goals. |
| ev_points | integer | Even-strength points. |
| faceoff_win_pct | numeric | Faceoff win percentage. |
| game_winning_goals | integer | Game-winning goals. |
| games_played | integer | Games played. |
| goals | integer | Goals scored. |
| last_name | character | Player last name. |
| ot_goals | integer | Overtime goals. |
| penalty_minutes | integer | Penalty minutes. |
| player_id | integer | Unique player identifier. |
| plus_minus | integer | Plus/minus rating. |
| points | integer | Total points (goals + assists). |
| points_per_game | numeric | Points per game. |
| position_code | character | Player position code. |
| pp_goals | integer | Power-play goals. |
| pp_points | integer | Power-play points. |
| season_id | integer | Season identifier. |
| sh_goals | integer | Short-handed goals. |
| sh_points | integer | Short-handed points. |
| shooting_pct | numeric | Shooting percentage. |
| shoots_catches | character | Handedness (shoots/catches). |
| shots | integer | Shots on goal. |
| skater_full_name | character | Player full name. |
| team_abbrevs | character | Team abbreviation(s). |
| time_on_ice_per_game | numeric | Average time on ice per game. |
| season | character | Season the row came from (e.g., "20232024"). |

Returns `NULL` on outer failure.

## Examples

``` r
# \donttest{
  try(nhl_skater_summary_range(start_season = 2023, end_season = 2024))
#> ── NHL Skater Summary Range ─────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 16:36:51 UTC
#> # A tibble: 100 × 27
#>    assists ev_goals ev_points faceoff_win_pct game_winning_goals games_played
#>      <int>    <int>     <int>           <dbl>              <int>        <int>
#>  1      89       39        75           0.519                 11           82
#>  2      76       19        64           0.549                 11           80
#>  3      52       43        76           0.421                 13           82
#>  4      83       22        63           1                      4           82
#>  5      69       30        77           0.444                  9           71
#>  6      69       26        72           0.448                  6           79
#>  7      63       33        68           0                      7           82
#>  8      50       42        68           0.490                  9           82
#>  9      67       21        47           0.452                  3           82
#> 10      63       28        68           0.443                  6           80
#> # ℹ 90 more rows
#> # ℹ 21 more variables: goals <int>, last_name <chr>, ot_goals <int>,
#> #   penalty_minutes <int>, player_id <int>, plus_minus <int>, points <int>,
#> #   points_per_game <dbl>, position_code <chr>, pp_goals <int>,
#> #   pp_points <int>, season_id <int>, sh_goals <int>, sh_points <int>,
#> #   shooting_pct <dbl>, shoots_catches <chr>, shots <int>,
#> #   skater_full_name <chr>, team_abbrevs <chr>, time_on_ice_per_game <dbl>, …
# }
```
