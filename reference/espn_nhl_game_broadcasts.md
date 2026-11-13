# **Get ESPN NHL Game Broadcasts (core-v2)**

**Get ESPN NHL Game Broadcasts (core-v2)**

## Usage

``` r
espn_nhl_game_broadcasts(event_id, cid = event_id, ...)
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
    espn_nhl_game_broadcasts(event_id = sb$game_id[1])
  })
#> ── NHL Game Broadcasts data from ESPN core-v2 ───────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:26:53 UTC
#> # A tibble: 3 × 20
#>   event_id  cid       type_id type_short_name type_long_name type_slug channel
#>   <chr>     <chr>     <chr>   <chr>           <chr>          <chr>       <int>
#> 1 401688263 401688263 1       TV              Television     tv            825
#> 2 401688263 401688263 1       TV              Television     tv           1125
#> 3 401688263 401688263 1       TV              Television     tv            399
#> # ℹ 13 more variables: station <chr>, slug <chr>, priority <int>, lang <chr>,
#> #   region <chr>, partnered <lgl>, market_id <chr>, market_type <chr>,
#> #   media_id <chr>, media_name <chr>, media_short_name <chr>,
#> #   media_call_letters <chr>, media_slug <chr>
# }
```
