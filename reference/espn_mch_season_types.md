# **Get ESPN MCH / WCH Season Types (core-v2)**

**Get ESPN MCH / WCH Season Types (core-v2)**

## Usage

``` r
espn_mch_season_types(season = most_recent_nhl_season(), ...)

espn_wch_season_types(season = most_recent_nhl_season(), ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per season type (pre-season,
regular, post-season, off-season):

|                |           |                                               |
|----------------|-----------|-----------------------------------------------|
| col_name       | type      | description                                   |
| ref            | character | `$ref` URL for the season-type object.        |
| season_type_id | integer   | Season type id (1=pre, 2=reg, 3=post, 4=off). |
| season         | integer   | Season year (echoed from arg).                |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_types()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_types.md);
columns verified against a captured MCH body.

`espn_wch_season_types()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_season_types(season = 2026))
#> ── MENS-COLLEGE-HOCKEY Season Types data from ESPN core-v2 ─────────────────────
#> ℹ Data updated: 2026-11-09 11:49:32 UTC
#> # A tibble: 2 × 3
#>   ref                                                      season_type_id season
#>   <chr>                                                             <int>  <int>
#> 1 http://sports.core.api.espn.com/v2/sports/hockey/league…              2   2026
#> 2 http://sports.core.api.espn.com/v2/sports/hockey/league…              3   2026
# }
# \donttest{
  try(espn_wch_season_types(season = 2026))
#> ── WOMENS-COLLEGE-HOCKEY Season Types data from ESPN core-v2 ───────────────────
#> ℹ Data updated: 2026-11-09 11:49:32 UTC
#> # A tibble: 2 × 3
#>   ref                                                      season_type_id season
#>   <chr>                                                             <int>  <int>
#> 1 http://sports.core.api.espn.com/v2/sports/hockey/league…              2   2026
#> 2 http://sports.core.api.espn.com/v2/sports/hockey/league…              3   2026
# }
```
