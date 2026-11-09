# **Get ESPN MCH / WCH Game Team Record (core-v2)**

**Get ESPN MCH / WCH Game Team Record (core-v2)**

## Usage

``` r
espn_mch_game_team_record(event_id, cid = event_id, team_id, ...)

espn_wch_game_team_record(event_id, cid = event_id, team_id, ...)
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

A `fastRhockey_data` tibble with one row per record type (e.g. overall,
home, road). **Note:** this endpoint returns 404 for most MCH games
tested; an empty
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) is returned in
that case.

|               |           |                                                    |
|---------------|-----------|----------------------------------------------------|
| col_name      | types     | description                                        |
| event_id      | character | ESPN event id (echoed from arg).                   |
| cid           | character | Competition id (echoed from arg).                  |
| team_id       | character | ESPN team id (echoed from arg).                    |
| name          | character | Record type name (e.g. "overall", "home", "road"). |
| abbreviation  | character | Record type abbreviation.                          |
| type          | character | Record type slug.                                  |
| summary       | character | Record summary string (e.g. "25-15-10").           |
| display_value | character | Record display value.                              |
| value         | numeric   | Record numeric value (e.g. win percentage).        |
| record_ref    | character | `$ref` URL for this record item.                   |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_team_record()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_team_record.md)
(columns inherited); a collection may be empty.

`espn_wch_game_team_record()` is the same wrapper for women's college
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
    espn_mch_game_team_record(event_id = eid, team_id = tms$team_id[1])
  })
#> ✖ 2026-11-09 22:50:26.345308: Invalid arguments or no ESPN mens-college-hockey game record for event 401715296 / team 194 available!
#> ✖ Args: league = "mens-college-hockey", event_id = "401715296", cid = "401715296", team_id = "194"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb  <- espn_wch_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    tms <- espn_wch_game_teams(event_id = eid)
    espn_wch_game_team_record(event_id = eid, team_id = tms$team_id[1])
  })
#> ✖ 2026-11-09 22:50:26.403361: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> ✖ 2026-11-09 22:50:26.439738: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL, team_id = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
