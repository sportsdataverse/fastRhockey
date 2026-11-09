# **Get ESPN MCH / WCH Game Play Personnel (core-v2)**

**Get ESPN MCH / WCH Game Play Personnel (core-v2)**

## Usage

``` r
espn_mch_game_play_personnel(event_id, cid = event_id, play_id, ...)

espn_wch_game_play_personnel(event_id, cid = event_id, play_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- play_id:

  ESPN play identifier.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per (competitor, athlete) pair
on the play. For MCH this endpoint is typically sparse: the competitor
block is present but the `entries` array is empty, yielding one row per
team with `NA` athlete fields. Returns an empty
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) if the endpoint
is not available.

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| play_id | character | ESPN play id (echoed from arg). |
| competitor_id | character | ESPN team/competitor id for this personnel block. |
| competitor_ref | character | `$ref` URL for the competitor object. |
| athlete_ref | character | `$ref` URL for the athlete (NA if entries are empty). |
| athlete_id | character | Athlete id parsed from the `$ref` (NA if entries empty). |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_play_personnel()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_play_personnel.md)
(columns inherited); a collection may be empty.

`espn_wch_game_play_personnel()` is the same wrapper for women's college
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
    pls <- espn_mch_game_plays(event_id = eid)
    espn_mch_game_play_personnel(event_id = eid, play_id = pls$id[1])
  })
#> ! 2026-11-09 17:18:18.682494: No plays returned for event 401715296.
#> ✖ 2026-11-09 17:18:18.69823: 
#> ✖ Args: league = "mens-college-hockey", event_id = "401715296", cid = "401715296", play_id = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb  <- espn_wch_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    pls <- espn_wch_game_plays(event_id = eid)
    espn_wch_game_play_personnel(event_id = eid, play_id = pls$id[1])
  })
#> ✖ 2026-11-09 17:18:18.75031: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL, limit = 1000
#> ✖ Error: The API returned an error
#> ✖ 2026-11-09 17:18:18.771682: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL, play_id = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
