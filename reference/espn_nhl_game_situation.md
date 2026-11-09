# **Get ESPN NHL Game Situation (core-v2)**

**Get ESPN NHL Game Situation (core-v2)**

## Usage

``` r
espn_nhl_game_situation(event_id, cid = event_id, ...)
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
    espn_nhl_game_situation(event_id = sb$game_id[1])
  })
#> ── NHL Game Situation data from ESPN core-v2 ────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:50 UTC
#> # A tibble: 1 × 6
#>   event_id  cid       power_play empty_net last_play_ref           situation_ref
#>   <chr>     <chr>     <lgl>      <lgl>     <chr>                   <chr>        
#> 1 401688263 401688263 FALSE      FALSE     http://sports.core.api… http://sport…
# }
```
