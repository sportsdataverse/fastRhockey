# Series, seasons (1975/76 on) and game types with their uuids, plus the default ssgt

Wraps
`GET https://www.shl.se/api/sports-v2/season-series-game-types-filter`
of the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_season_filter(series_uuid = NULL, season_uuid = NULL, return_parsed = TRUE)
```

## Arguments

- series_uuid:

  Series uuid (SHL qQ9-bb0bzEWUk, CHL qQ9-9397drVEh). No effect on the
  body in the drop-one probe.

- season_uuid:

  Season uuid; picks the default ssgt returned for that season (omit for
  the current season).

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| kind | character | Directory entry kind: series, season or game_type. |
| code | character | Game type code: regular, playoff, qualdown (legacy career logs: qualification). |
| name | character | Game type label in Swedish: Grundserie = regular season, Slutspel = playoffs, Kvalspel nedflyttning = relegation qualification, Kvalserien = the old promotion/relegation series. |
| uuid | character | Game type uuid (platform key space; regular qQ9-af37Ti40B); the gameTypeUuid param of game-schedule. |
| deleted | logical | Soft-delete flag of the record (false in every sample). |

## Errors

`fastRhockey_no_data`: the host answered 404, an empty body (gameday
routes before 2023/24), a table with no rows, or the season and game
type are not in layout-info. `fastRhockey_asset_fetch`: any other
non-2xx answer or a dead connection after retries, a non-JSON or
error-envelope 200, a stats-info page that reaches `count` or falls
short of its `totalCount`, or a body for another game, team, athlete or
ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
season that is not a year, or an argument outside its choices.

## See also

[family_shl](https://fastRhockey.sportsdataverse.org/reference/family_shl.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(shl_season_filter(series_uuid = "qQ9-bb0bzEWUk", season_uuid = "xs4m9qupsi"))
#> ── SHL Season Filter Information from www.shl.se ────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:23:55 UTC
#> # A tibble: 57 × 5
#>    kind   code  name      uuid           deleted
#>    <chr>  <chr> <chr>     <chr>          <lgl>  
#>  1 series CHL   CHL       qQ9-9397drVEh  FALSE  
#>  2 series SHL   SHL       qQ9-bb0bzEWUk  FALSE  
#>  3 season 2026  2026/2027 ndcf81nlb3     FALSE  
#>  4 season 2025  2025/2026 xs4m9qupsi     FALSE  
#>  5 season 2024  2024/2025 qeb-73bZkIm9A  FALSE  
#>  6 season 2023  2023/2024 qcz-3NvSZ2Cmh  FALSE  
#>  7 season 2022  2022/2023 qbN-XMFfjGVt   FALSE  
#>  8 season 2021  2021/2022 qZl-8qa6OaFXf  FALSE  
#>  9 season 2020  2020/2021 qY7-AdVh5z1XJ  FALSE  
#> 10 season 2019  2019/2020 qWX-334j11U5o1 FALSE  
#> # ℹ 47 more rows
# }
```
