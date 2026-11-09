# **Get ESPN NHL Game Competition Detail (core-v2)**

**Get ESPN NHL Game Competition Detail (core-v2)**

## Usage

``` r
espn_nhl_game_competition(event_id, cid = event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id` (for NHL the single
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
    espn_nhl_game_competition(event_id = sb$game_id[1])
  })
#> ── NHL Game Competition data from ESPN core-v2 ──────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:27 UTC
#> # A tibble: 1 × 20
#>   event_id  cid       id    guid  uid   date  attendance neutral_site time_valid
#>   <chr>     <chr>     <chr> <chr> <chr> <chr>      <int> <lgl>        <lgl>     
#> 1 401688263 401688263 4016… 82db… s:70… 2025…      19515 FALSE        TRUE      
#> # ℹ 11 more variables: boxscore_available <lgl>, play_by_play_available <lgl>,
#> #   linescore_available <lgl>, on_watch_espn <lgl>, recent <lgl>,
#> #   venue_ref <chr>, competitors_ref <chr>, status_ref <chr>, odds_ref <chr>,
#> #   officials_ref <chr>, competition_ref <chr>
# }
```
