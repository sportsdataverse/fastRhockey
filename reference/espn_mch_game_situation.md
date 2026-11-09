# **Get ESPN MCH / WCH Game Situation (core-v2)**

**Get ESPN MCH / WCH Game Situation (core-v2)**

## Usage

``` r
espn_mch_game_situation(event_id, cid = event_id, ...)

espn_wch_game_situation(event_id, cid = event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with the game state at the moment of
the last tracked play:

|               |           |                                              |
|---------------|-----------|----------------------------------------------|
| col_name      | types     | description                                  |
| event_id      | character | ESPN event id (echoed from arg).             |
| cid           | character | Competition id (echoed from arg).            |
| power_play    | logical   | Whether a power play was in effect.          |
| empty_net     | logical   | Whether the net was empty.                   |
| last_play_ref | character | `$ref` URL for the last tracked play object. |
| situation_ref | character | `$ref` URL for this situation object.        |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_situation()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_situation.md)
(columns inherited); a collection may be empty.

`espn_wch_game_situation()` is the same wrapper for women's college
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
    espn_mch_game_situation(event_id = sb$game_id[1])
  })
#> ── MENS-COLLEGE-HOCKEY Game Situation data from ESPN core-v2 ───────────────────
#> ℹ Data updated: 2026-11-09 17:18:22 UTC
#> # A tibble: 1 × 6
#>   event_id  cid       power_play empty_net last_play_ref situation_ref          
#>   <chr>     <chr>     <lgl>      <lgl>     <chr>         <chr>                  
#> 1 401715296 401715296 NA         NA        NA            http://sports.core.api…
# }
# \donttest{
  try({
    sb <- espn_wch_scoreboard(dates = "20250110")
    espn_wch_game_situation(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-09 17:18:22.453903: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
