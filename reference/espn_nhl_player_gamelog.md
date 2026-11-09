# **Get ESPN NHL Player Gamelog**

**Get ESPN NHL Player Gamelog**

## Usage

``` r
espn_nhl_player_gamelog(athlete_id, season = NULL, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric, e.g. `"5149125"`).
  Obtain via
  [`espn_nhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_roster.md).

- season:

  Season end-year (e.g. `2025`). When `NULL` ESPN returns the current
  season. Defaults to `NULL`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per game, or an empty
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) when no data is
available (e.g. if the endpoint returns HTTP 404 in a future ESPN schema
change). If the endpoint becomes unavailable, use
[`espn_nhl_player_statisticslog()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_statisticslog.md)
from the core-v2 host as a reliable substitute.

|  |  |  |
|----|----|----|
| col_name | types | description |
| athlete_id | character | ESPN athlete identifier (echoed from arg). |
| event_id | character | ESPN event identifier. |
| season_type_display | character | Season type label (e.g. "2025-26 Regular Season"). |
| category_display | character | Stat category display name. |
| category_type | character | Stat category type. |
| category_split_type | character | Stat category split type. |
| game_date | character | Game date-time (ISO 8601). |
| at_vs | character | "at" or "vs" home/away indicator. |
| score | character | Final score string. |
| home_team_id | character | ESPN home team identifier. |
| away_team_id | character | ESPN away team identifier. |
| home_team_score | character | Home team final score. |
| away_team_score | character | Away team final score. |
| game_result | character | Game result for the player's team (W/L/OT). |
| opponent_id | character | ESPN opponent team identifier. |
| opponent_name | character | Opponent team display name. |
| opponent_abbr | character | Opponent team abbreviation. |
| team_id | character | Player's team ESPN identifier. |
| team_abbr | character | Player's team abbreviation. |
| league_name | character | League name. |
| league_abbreviation | character | League abbreviation. |
| goals | character | Goals scored. |
| assists | character | Assists. |
| points | character | Points (G+A). |
| plus_minus | character | Plus/minus. |
| penalty_minutes | character | Penalty minutes. |
| shots_total | character | Shots on goal. |
| shooting_pct | character | Shooting percentage. |
| power_play_goals | character | Power-play goals. |
| power_play_assists | character | Power-play assists. |
| short_handed_goals | character | Short-handed goals. |
| short_handed_assists | character | Short-handed assists. |
| game_winning_goals | character | Game-winning goals. |
| time_on_ice_per_game | character | Average time on ice. |
| production | character | Production metric. |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_gamelog(athlete_id = "5149125"))
#> ── NHL Player Gamelog data from ESPN.com ────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:59 UTC
#> # A tibble: 12 × 35
#>    athlete_id event_id  season_type_display    category_display category_type
#>    <chr>      <chr>     <chr>                  <chr>            <chr>        
#>  1 5149125    401892668 2026-27 Regular Season November         event        
#>  2 5149125    401892650 2026-27 Regular Season November         event        
#>  3 5149125    401892645 2026-27 Regular Season November         event        
#>  4 5149125    401892631 2026-27 Regular Season November         event        
#>  5 5149125    401892614 2026-27 Regular Season October          event        
#>  6 5149125    401892600 2026-27 Regular Season October          event        
#>  7 5149125    401892587 2026-27 Regular Season October          event        
#>  8 5149125    401892578 2026-27 Regular Season October          event        
#>  9 5149125    401892564 2026-27 Regular Season October          event        
#> 10 5149125    401892528 2026-27 Regular Season October          event        
#> 11 5149125    401892512 2026-27 Regular Season October          event        
#> 12 5149125    401892496 2026-27 Regular Season October          event        
#> # ℹ 30 more variables: category_split_type <chr>, game_date <chr>, at_vs <chr>,
#> #   score <chr>, home_team_id <chr>, away_team_id <chr>, home_team_score <chr>,
#> #   away_team_score <chr>, game_result <chr>, opponent_id <chr>,
#> #   opponent_name <chr>, opponent_abbr <chr>, team_id <chr>, team_abbr <chr>,
#> #   league_name <chr>, league_abbreviation <chr>, goals <chr>, assists <chr>,
#> #   points <chr>, plus_minus <chr>, penalty_minutes <chr>, shots_total <chr>,
#> #   shooting_pct <chr>, power_play_goals <chr>, power_play_assists <chr>, …
# }
```
