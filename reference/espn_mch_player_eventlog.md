# **Get ESPN MCH / WCH Player Event Log (core-v2)**

**Get ESPN MCH / WCH Player Event Log (core-v2)**

## Usage

``` r
espn_mch_player_eventlog(athlete_id, ...)

espn_wch_player_eventlog(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per event (game) in the current
season. Use `statistics_ref` to retrieve per-game statistics.

|                 |           |                                               |
|-----------------|-----------|-----------------------------------------------|
| col_name        | types     | description                                   |
| athlete_id      | character | ESPN athlete id (echoed from arg).            |
| event_id        | character | ESPN event (game) id.                         |
| team_id         | character | Team id the athlete played for in this event. |
| played          | logical   | Whether the athlete played in the event.      |
| event_ref       | character | `$ref` URL for the event object.              |
| competition_ref | character | `$ref` URL for the competition object.        |
| statistics_ref  | character | `$ref` URL for per-game athlete statistics.   |
| count           | integer   | Total event count for this athlete.           |
| page_count      | integer   | Total page count for this endpoint.           |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_player_eventlog()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_eventlog.md)
(columns inherited); a collection may be empty.

`espn_wch_player_eventlog()` is the same wrapper for women's college
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
  try(espn_mch_player_eventlog(athlete_id = "5136656"))
#> ✖ 2026-11-13 21:26:23.343958: Invalid arguments or no ESPN mens-college-hockey player eventlog for athlete 5136656 available!
#> ✖ Args: league = "mens-college-hockey", athlete_id = "5136656"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_player_eventlog(athlete_id = "5263060"))
#> ✖ 2026-11-13 21:26:23.380922: Invalid arguments or no ESPN womens-college-hockey player eventlog for athlete 5263060 available!
#> ✖ Args: league = "womens-college-hockey", athlete_id = "5263060"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
