# **Get ESPN MCH / WCH Game Leaders (core-v2)**

**Get ESPN MCH / WCH Game Leaders (core-v2)**

## Usage

``` r
espn_mch_game_leaders(event_id, cid = event_id, ...)

espn_wch_game_leaders(event_id, cid = event_id, ...)
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
**Note:** This endpoint is sparse for MCH - most games return an empty
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_leaders.md)
(columns inherited); a collection may be empty.

`espn_wch_game_leaders()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb <- espn_mch_scoreboard(dates = "20250110")
    espn_mch_game_leaders(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-09 19:33:39.376908: Invalid arguments or no ESPN mens-college-hockey game leaders for event 401715296 available!
#> ✖ Args: league = "mens-college-hockey", event_id = "401715296", cid = "401715296"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb <- espn_wch_scoreboard(dates = "20250110")
    espn_wch_game_leaders(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-09 19:33:39.421033: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
