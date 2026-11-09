# **Get ESPN MCH / WCH Game Team Leaders (core-v2)**

**Get ESPN MCH / WCH Game Team Leaders (core-v2)**

## Usage

``` r
espn_mch_game_team_leaders(event_id, cid = event_id, team_id, ...)

espn_wch_game_team_leaders(event_id, cid = event_id, team_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- team_id:

  ESPN team/competitor identifier. Use
  `espn_mch_game_teams(event_id)$team_id` to find valid team ids for a
  game.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per (category, leader). For a
typical completed MCH game there are 6 stat categories (goals, assists,
points, plusMinus, penaltyMinutes, saves) with one leader each, yielding
~6 rows per team:

|                  |           |                                       |
|------------------|-----------|---------------------------------------|
| col_name         | types     | description                           |
| event_id         | character | ESPN event id (echoed from arg).      |
| cid              | character | Competition id (echoed from arg).     |
| team_id          | character | ESPN team id (echoed from arg).       |
| category_name    | character | Stat category name (e.g. "goals").    |
| category_display | character | Stat category display name.           |
| category_short   | character | Stat category short display name.     |
| category_abbr    | character | Stat category abbreviation.           |
| display_value    | character | Leader stat display value (e.g. "2"). |
| value            | numeric   | Leader stat numeric value.            |
| athlete_ref      | character | `$ref` URL for the leading athlete.   |
| athlete_id       | character | Athlete id parsed from the `$ref`.    |
| team_ref         | character | `$ref` URL for the athlete's team.    |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_team_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_team_leaders.md)
(columns inherited); a collection may be empty.

`espn_wch_game_team_leaders()` is the same wrapper for women's college
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
  try({
    sb  <- espn_mch_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    tms <- espn_mch_game_teams(event_id = eid)
    espn_mch_game_team_leaders(event_id = eid, team_id = tms$team_id[1])
  })
#> ✖ 2026-11-09 22:50:25.105773: Invalid arguments or no ESPN mens-college-hockey game leaders for event 401715296 / team 194 available!
#> ✖ Args: league = "mens-college-hockey", event_id = "401715296", cid = "401715296", team_id = "194"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb  <- espn_wch_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    tms <- espn_wch_game_teams(event_id = eid)
    espn_wch_game_team_leaders(event_id = eid, team_id = tms$team_id[1])
  })
#> ✖ 2026-11-09 22:50:25.162318: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> ✖ 2026-11-09 22:50:25.197914: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL, team_id = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
