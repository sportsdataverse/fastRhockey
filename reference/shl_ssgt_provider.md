# Stats provider of an ssgt, a bare string such as statnet

Wraps `GET https://www.shl.se/api/sports-v2/ssgt/{ssgt_uuid}/provider`
of the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_ssgt_provider(ssgt_uuid, return_parsed = TRUE)
```

## Arguments

- ssgt_uuid:

  ssgt uuid: one season + series + game type (e.g. 2025/2026 SHL regular
  season). Directory: /statistics-v2/layout-info
  originalSeasonSeriesGameTypeTeams\[\].ssgtUuid.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|          |           |                                               |
|----------|-----------|-----------------------------------------------|
| col_name | type      | description                                   |
| provider | character | Stats provider of the ssgt (statnet, impleo). |

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
  try(shl_ssgt_provider(ssgt_uuid = "iuzqg7dqk9"))
#> ── SHL Ssgt Provider Information from www.shl.se ────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:23:13 UTC
#> # A tibble: 1 × 1
#>   provider
#>   <chr>   
#> 1 statnet 
# }
```
