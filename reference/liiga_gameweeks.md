# **Liiga game weeks**

Number of game weeks of a season and tournament.

## Usage

``` r
liiga_gameweeks(season = NULL, tournament = "runkosarja")
```

## Arguments

- season:

  Season END year (2026 = 2025-26). Omitted = the season in progress.

- tournament:

  Tournament: runkosarja (regular season), playoffs, playout,
  qualifications, valmistavat_ottelut (preseason) or chl (Champions
  Hockey League). Required by the API; defaults to runkosarja.

## Value

A `fastRhockey_data` tibble (3 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `season` | integer | Season END year (2026 = 2025-26). |
| `serie` | character | Tournament code, uppercase (RUNKOSARJA, PLAYOFFS, PRACTICE, ...); in /schedule the integer tournament id instead (see serieId and Divergence notes). |
| `nb_weeks` | integer | Number of game weeks. |

## Details

Endpoint: `GET https://liiga.fi/api/v2/gameweeks`

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
  try(liiga_gameweeks(season = 2026))
#> ── Liiga game weeks Information from liiga.fi ───────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:51:29 UTC
#> # A tibble: 1 × 3
#>   season serie      nb_weeks
#>    <int> <chr>         <int>
#> 1   2026 RUNKOSARJA       27
# }
```
