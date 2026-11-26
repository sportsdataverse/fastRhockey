# **Get ESPN MCH / WCH Game Team Statistics (core-v2)**

**Get ESPN MCH / WCH Game Team Statistics (core-v2)**

## Usage

``` r
espn_mch_game_team_statistics(event_id, cid = event_id, team_id, ...)

espn_wch_game_team_statistics(event_id, cid = event_id, team_id, ...)
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
stat in each category. For a typical completed MCH game there are 5 stat
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_team_statistics()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_team_statistics.md)
(columns inherited); a collection may be empty.

`espn_wch_game_team_statistics()` is the same wrapper for women's
college hockey (league slug `womens-college-hockey`): same arguments,
same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb  <- espn_mch_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    tms <- espn_mch_game_teams(event_id = eid)
    espn_mch_game_team_statistics(event_id = eid, team_id = tms$team_id[1])
  })
#> ✖ 2026-11-26 22:24:06.458003: Invalid arguments or no ESPN mens-college-hockey game team statistics for event 401715296 / team 194 available!
#> ✖ Args: league = "mens-college-hockey", event_id = "401715296", cid = "401715296", team_id = "194"
#> ✖ Error: HTTP 404 Not Found from https://sports.core.api.espn.com/v2/sports/hockey/leagues/mens-college-hockey/events/401715296/competitions/401715296/competitors/194/statistics
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb  <- espn_wch_scoreboard(dates = "20260110")
    eid <- sb$game_id[1]
    tms <- espn_wch_game_teams(event_id = eid)
    espn_wch_game_team_statistics(event_id = eid, team_id = tms$team_id[1])
  })
#> ✖ 2026-11-26 22:24:06.674012: Invalid arguments or no ESPN womens-college-hockey game team statistics for event 401814005 / team 2385 available!
#> ✖ Args: league = "womens-college-hockey", event_id = "401814005", cid = "401814005", team_id = "2385"
#> ✖ Error: HTTP 404 Not Found from https://sports.core.api.espn.com/v2/sports/hockey/leagues/womens-college-hockey/events/401814005/competitions/401814005/competitors/2385/statistics
#> data frame with 0 columns and 0 rows
# }
```
