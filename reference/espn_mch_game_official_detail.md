# **Get ESPN MCH / WCH Game Official Detail (core-v2)**

**Get ESPN MCH / WCH Game Official Detail (core-v2)**

## Usage

``` r
espn_mch_game_official_detail(event_id, cid = event_id, official_id, ...)

espn_wch_game_official_detail(event_id, cid = event_id, official_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- official_id:

  ESPN official identifier. Use
  `espn_mch_game_officials(event_id)$official_id` to find valid official
  ids for a game.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with official detail:

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| official_id | character | ESPN official id (echoed from arg). |
| id | character | ESPN official identifier. |
| first_name | character | Official first name. |
| last_name | character | Official last name. |
| full_name | character | Official full name. |
| display_name | character | Official display name. |
| order | integer | Display order within officials list. |
| position_id | character | Official position identifier. |
| position_name | character | Official position name (e.g. "Linesman", "Referee"). |
| position_display | character | Official position display name. |
| official_ref | character | `$ref` URL for this official object. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_official_detail()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_official_detail.md)
(columns inherited); a collection may be empty.

`espn_wch_game_official_detail()` is the same wrapper for women's
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
    sb   <- espn_mch_scoreboard(dates = "20250110")
    eid  <- sb$game_id[1]
    offs <- espn_mch_game_officials(event_id = eid)
    espn_mch_game_official_detail(event_id = eid, official_id = offs$official_id[1])
  })
#> ! 2026-11-09 19:33:40.646178: No officials returned for event 401715296.
#> ✖ 2026-11-09 19:33:40.663698: 
#> ✖ Args: league = "mens-college-hockey", event_id = "401715296", cid = "401715296", official_id = NULL
#> ✖ Error: arguments imply differing number of rows: 1, 0
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb   <- espn_wch_scoreboard(dates = "20250110")
    eid  <- sb$game_id[1]
    offs <- espn_wch_game_officials(event_id = eid)
    espn_wch_game_official_detail(event_id = eid, official_id = offs$official_id[1])
  })
#> ✖ 2026-11-09 19:33:40.703012: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> ✖ 2026-11-09 19:33:40.722348: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL, official_id = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
