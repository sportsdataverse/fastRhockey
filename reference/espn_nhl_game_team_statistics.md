# **Get ESPN NHL Game Team Statistics (core-v2)**

**Get ESPN NHL Game Team Statistics (core-v2)**

## Usage

``` r
espn_nhl_game_team_statistics(event_id, cid = event_id, team_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- team_id:

  ESPN team/competitor identifier.

- ...:

  Reserved for forward compatibility.

## Value

A wide one-row `fastRhockey_data` tibble. The first three columns are
always present; the remaining stat columns are named `<category>_<stat>`
(display value) and `<category>_<stat>_value` (numeric value) for each
stat in each category. For a typical completed NHL game there are 5 stat
categories (defensive, general, offensive, specialTeams, goalie)
producing ~30 column pairs:

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| team_id | character | ESPN team id (echoed from arg). |
| category_stat | character | Display value for a stat within a category (e.g. offensive_goals). |
| category_stat_value | numeric | Numeric value for a stat within a category (e.g. offensive_goals_value). |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb  <- espn_nhl_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    tms <- espn_nhl_game_teams(event_id = eid)
    espn_nhl_game_team_statistics(event_id = eid, team_id = tms$team_id[1])
  })
#> ── NHL Game Team Statistics data from ESPN core-v2 ──────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:36 UTC
#> # A tibble: 1 × 135
#>   event_id  cid       team_id def_goals_against def_goals_against_value
#>   <chr>     <chr>     <chr>   <chr>                               <dbl>
#> 1 401688263 401688263 5       3                                       3
#> # ℹ 130 more variables: def_shots_against <chr>, def_shots_against_value <dbl>,
#> #   def_penalty_kill_pct <chr>, def_penalty_kill_pct_value <dbl>,
#> #   def_power_play_goals_against <chr>,
#> #   def_power_play_goals_against_value <dbl>,
#> #   def_short_handed_goals_against <chr>,
#> #   def_short_handed_goals_against_value <dbl>, def_shootout_saves <chr>,
#> #   def_shootout_saves_value <dbl>, def_shootout_shots_against <chr>, …
# }
```
