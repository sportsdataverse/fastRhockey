# **Get ESPN MCH / WCH Game Competition Detail (core-v2)**

**Get ESPN MCH / WCH Game Competition Detail (core-v2)**

## Usage

``` r
espn_mch_game_competition(event_id, cid = event_id, ...)

espn_wch_game_competition(event_id, cid = event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id` (for MCH the single
  competition always has the same id as its parent event).

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with competition detail:

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| id | character | ESPN competition identifier. |
| guid | character | Competition global unique identifier. |
| uid | character | Competition uid string. |
| date | character | Start date/time (ISO 8601 UTC). |
| attendance | integer | Announced attendance. |
| neutral_site | logical | Whether the game was played at a neutral venue. |
| time_valid | logical | Whether the start time is confirmed. |
| boxscore_available | logical | Whether boxscore data is available. |
| play_by_play_available | logical | Whether play-by-play data is available. |
| linescore_available | logical | Whether linescore data is available. |
| on_watch_espn | logical | Whether the game is/was on WatchESPN. |
| recent | logical | Whether the game is recent. |
| venue_ref | character | `$ref` URL for the venue. |
| competitors_ref | character | `$ref` URL for the competitors collection. |
| status_ref | character | `$ref` URL for the competition status object. |
| odds_ref | character | `$ref` URL for the odds collection. |
| officials_ref | character | `$ref` URL for the officials collection. |
| competition_ref | character | `$ref` URL for this competition object. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_competition()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_competition.md)
(columns inherited); a collection may be empty.

`espn_wch_game_competition()` is the same wrapper for women's college
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
    espn_mch_game_competition(event_id = sb$game_id[1])
  })
#> ── MENS-COLLEGE-HOCKEY Game Competition data from ESPN core-v2 ─────────────────
#> ℹ Data updated: 2026-11-13 21:26:05 UTC
#> # A tibble: 1 × 20
#>   event_id  cid       id    guid  uid   date  attendance neutral_site time_valid
#>   <chr>     <chr>     <chr> <chr> <chr> <chr>      <int> <lgl>        <lgl>     
#> 1 401715296 401715296 4017… afb4… s:70… 2025…          0 FALSE        TRUE      
#> # ℹ 11 more variables: boxscore_available <lgl>, play_by_play_available <lgl>,
#> #   linescore_available <lgl>, on_watch_espn <lgl>, recent <lgl>,
#> #   venue_ref <chr>, competitors_ref <chr>, status_ref <chr>, odds_ref <chr>,
#> #   officials_ref <chr>, competition_ref <chr>
# }
# \donttest{
  try({
    sb <- espn_wch_scoreboard(dates = "20250110")
    espn_wch_game_competition(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-13 21:26:05.882143: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
