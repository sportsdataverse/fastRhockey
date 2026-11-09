# **Get ESPN MCH / WCH Player Gamelog**

**Get ESPN MCH / WCH Player Gamelog**

## Usage

``` r
espn_mch_player_gamelog(athlete_id, season = NULL, ...)

espn_wch_player_gamelog(athlete_id, season = NULL, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric, e.g. `"5136656"`).
  Obtain via
  [`espn_mch_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_roster.md).

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
[`espn_mch_player_statisticslog()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_statisticslog.md)
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_player_gamelog()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_gamelog.md)
(columns inherited); a collection may be empty.

`espn_wch_player_gamelog()` is the same wrapper for women's college
hockey (league slug `womens-college-hockey`): same arguments, same
columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_player_gamelog(athlete_id = "5136656"))
#> ! ESPN web-v3 gamelog returned HTTP 404 for NHL athlete 5136656; use espn_nhl_player_statisticslog() instead.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_player_gamelog(athlete_id = "5263060"))
#> ! ESPN web-v3 gamelog returned HTTP 404 for NHL athlete 5263060; use espn_nhl_player_statisticslog() instead.
#> data frame with 0 columns and 0 rows
# }
```
