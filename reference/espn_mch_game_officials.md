# **Get ESPN MCH / WCH Game Officials (core-v2)**

**Get ESPN MCH / WCH Game Officials (core-v2)**

## Usage

``` r
espn_mch_game_officials(event_id, cid = event_id, ...)

espn_wch_game_officials(event_id, cid = event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per official (typically 4 - 2
referees + 2 linesmen):

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| official_id | character | ESPN official identifier. |
| first_name | character | Official first name. |
| last_name | character | Official last name. |
| full_name | character | Official full name. |
| display_name | character | Official display name. |
| order | integer | Display order within officials list. |
| position_id | character | Official position identifier. |
| position_name | character | Official position name (e.g. "Referee", "Linesman"). |
| position_display | character | Official position display name. |
| official_ref | character | `$ref` URL for this official object. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_officials()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_officials.md)
(columns inherited); a collection may be empty.

`espn_wch_game_officials()` is the same wrapper for women's college
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
    sb <- espn_mch_scoreboard(dates = "20250110")
    espn_mch_game_officials(event_id = sb$game_id[1])
  })
#> ! 2026-11-09 17:18:17.634542: No officials returned for event 401715296.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb <- espn_wch_scoreboard(dates = "20250110")
    espn_wch_game_officials(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-09 17:18:17.66782: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
