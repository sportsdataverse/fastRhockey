# **Get ESPN MCH / WCH Game Broadcasts (core-v2)**

**Get ESPN MCH / WCH Game Broadcasts (core-v2)**

## Usage

``` r
espn_mch_game_broadcasts(event_id, cid = event_id, ...)

espn_wch_game_broadcasts(event_id, cid = event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per broadcast (regional TV +
national, typically 2-4 rows):

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| type_id | character | Broadcast type id. |
| type_short_name | character | Broadcast type short name (e.g. "TV"). |
| type_long_name | character | Broadcast type long name (e.g. "Television"). |
| type_slug | character | Broadcast type slug. |
| channel | integer | Channel number identifier. |
| station | character | Station full name (e.g. "FanDuel Sports Network Detroit"). |
| slug | character | Station slug. |
| priority | integer | Broadcast priority order. |
| lang | character | Broadcast language (e.g. "en"). |
| region | character | Broadcast region (e.g. "us"). |
| partnered | logical | Whether this is a partnered broadcast. |
| market_id | character | Market identifier. |
| market_type | character | Market type. |
| media_id | character | Media outlet identifier. |
| media_name | character | Media outlet full name. |
| media_short_name | character | Media outlet short name. |
| media_call_letters | character | Media outlet call letters. |
| media_slug | character | Media outlet slug. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_broadcasts()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_broadcasts.md)
(columns inherited); a collection may be empty.

`espn_wch_game_broadcasts()` is the same wrapper for women's college
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
    espn_mch_game_broadcasts(event_id = sb$game_id[1])
  })
#> ! 2026-11-13 21:26:05.268012: No broadcasts returned for event 401715296.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb <- espn_wch_scoreboard(dates = "20250110")
    espn_wch_game_broadcasts(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-13 21:26:05.31636: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
