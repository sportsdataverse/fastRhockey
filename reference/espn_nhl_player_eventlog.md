# **Get ESPN NHL Player Event Log (core-v2)**

**Get ESPN NHL Player Event Log (core-v2)**

## Usage

``` r
espn_nhl_player_eventlog(athlete_id, ...)
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_eventlog(athlete_id = "4024820"))
#> ✖ 2026-11-09 19:34:42.849224: Invalid arguments or no ESPN nhl player eventlog for athlete 4024820 available!
#> ✖ Args: league = "nhl", athlete_id = "4024820"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
