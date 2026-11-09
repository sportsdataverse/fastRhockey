# **Liiga tournament**

Current state of the league: game week of the season in progress and its
last game week.

## Usage

``` r
liiga_tournament()
```

## Value

A `fastRhockey_data` tibble (3 columns; zero rows when a valid payload
has none).

|                 |         |                                  |
|-----------------|---------|----------------------------------|
| col_name        | type    | description                      |
| `cachebust`     | logical | Cache-busting flag for the site. |
| `game_week`     | integer | Game week of the season.         |
| `max_game_week` | integer | Last game week of the season.    |

## Details

Endpoint: `GET https://liiga.fi/api/v2/tournament`

Seasons are END years (2026 = 2025-26), the same key as
sportsdataverse-py. Requests to liiga.fi are spaced at least one second
apart. A request Liiga answers with nothing (an unknown game, a
tournament not played that season, a shot map before 2016) raises
`fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
`fastRhockey_asset_fetch`; a bad argument raises
`fastRhockey_bad_request`.

## See also

[family_liiga](https://fastRhockey.sportsdataverse.org/reference/family_liiga.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(liiga_tournament())
#> ── Liiga tournament Information from liiga.fi ───────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:37:59 UTC
#> # A tibble: 1 × 3
#>   cachebust game_week max_game_week
#>   <lgl>         <int>         <int>
#> 1 FALSE            10            28
# }
```
