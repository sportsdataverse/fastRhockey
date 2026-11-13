# **Get ESPN NHL Game Team Record (core-v2)**

**Get ESPN NHL Game Team Record (core-v2)**

## Usage

``` r
espn_nhl_game_team_record(event_id, cid = event_id, team_id, ...)
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
home, road). **Note:** this endpoint returns 404 for most NHL games
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
    espn_nhl_game_team_record(event_id = eid, team_id = tms$team_id[1])
  })
#> ✖ 2026-11-13 20:16:34.907727: Invalid arguments or no ESPN nhl game record for event 401688263 / team 5 available!
#> ✖ Args: league = "nhl", event_id = "401688263", cid = "401688263", team_id = "5"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
