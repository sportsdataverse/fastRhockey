# Directory of every SHL ssgt (season + series + game type) with teams and the stats modules

Wraps `GET https://www.shl.se/api/statistics-v2/layout-info` of the SHL
site API (www.shl.se). Unofficial use of a public source; seasons are
END years (2026 = 2025/26) and are mapped to the site's ssgt uuids
through `/statistics-v2/layout-info`.

## Usage

``` r
shl_layout_info(return_parsed = TRUE)
```

## Arguments

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| season | integer | Season END year (2026 = 2025/26). |
| season_code | character | Provider season code: the START year as a string (2025 = 2025/26). |
| season_name | character | Season label (e.g. 2025/2026). |
| game_type | character | Game type record (uuid, code regular / playoff / qualdown, Swedish name). |
| game_type_name | character | Game type label in Swedish: Grundserie = regular season, Slutspel = playoffs, Kvalspel nedflyttning = relegation qualification, Kvalserien = the old promotion/relegation series. |
| series_code | character | Series code (SHL, CHL; HA and Kvalserien in old career logs). |
| ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info originalSeasonSeriesGameTypeTeams\[\]. |
| state | character | ssgt state: active (the current season) or closed. |
| season_uuid | character | Season uuid (platform key space; code 2025 = 2025/2026); the seasonUuid param of game-schedule. |
| series_uuid | character | Series uuid (platform key space; SHL = qQ9-bb0bzEWUk). |
| game_type_uuid | character | Game type uuid (platform key space; regular qQ9-af37Ti40B); the gameTypeUuid param of game-schedule. |

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
  try(shl_layout_info())
#> ── SHL Layout Info Information from www.shl.se ──────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:58:39 UTC
#> # A tibble: 112 × 11
#>    season season_code season_name game_type game_type_name series_code ssgt_uuid
#>     <int> <chr>       <chr>       <chr>     <chr>          <chr>       <chr>    
#>  1   2025 2024        2024/2025   playoff   Slutspel       SHL         fg2i6uu9…
#>  2   2026 2025        2025/2026   regular   Grundserie     SHL         iuzqg7dq…
#>  3   2026 2025        2025/2026   qualdown  Kvalspel nedf… SHL         jjruyvpq…
#>  4   2025 2024        2024/2025   qualdown  Kvalspel nedf… SHL         kr8hte1o…
#>  5   2027 2026        2026/2027   playoff   Slutspel       SHL         oet9x00s…
#>  6   2027 2026        2026/2027   qualdown  Kvalspel nedf… SHL         om4gjjxh…
#>  7   2027 2026        2026/2027   regular   Grundserie     SHL         qa98unlb…
#>  8   2022 2021        2021/2022   qualdown  Kvalspel nedf… SHL         qbK-6Iso…
#>  9   2022 2021        2021/2022   playoff   Slutspel       SHL         qbK-6ItY…
#> 10   2023 2022        2022/2023   regular   Grundserie     SHL         qbN-6KVf…
#> # ℹ 102 more rows
#> # ℹ 4 more variables: state <chr>, season_uuid <chr>, series_uuid <chr>,
#> #   game_type_uuid <chr>
# }
```
