# **Get ESPN NHL Game Leaders (core-v2)**

**Get ESPN NHL Game Leaders (core-v2)**

## Usage

``` r
espn_nhl_game_leaders(event_id, cid = event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per (category, leader) pair.
**Note:** This endpoint is sparse for NHL - most games return an empty
tibble with a warning. When populated:

|                  |           |                                    |
|------------------|-----------|------------------------------------|
| col_name         | types     | description                        |
| event_id         | character | ESPN event id (echoed from arg).   |
| cid              | character | Competition id (echoed from arg).  |
| category_name    | character | Stat category name.                |
| category_display | character | Stat category display name.        |
| category_abbr    | character | Stat category abbreviation.        |
| value            | numeric   | Leader stat value.                 |
| display_value    | character | Formatted stat value.              |
| athlete_ref      | character | `$ref` URL for the leader athlete. |
| team_ref         | character | `$ref` URL for the leader's team.  |
| leader_ref       | character | `$ref` URL for this leader entry.  |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb <- espn_nhl_scoreboard(dates = "20250110")
    espn_nhl_game_leaders(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-13 21:26:54.524042: Invalid arguments or no ESPN nhl game leaders for event 401688263 available!
#> ✖ Args: league = "nhl", event_id = "401688263", cid = "401688263"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
