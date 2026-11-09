# **Get ESPN MCH / WCH Seasons List (core-v2)**

**Get ESPN MCH / WCH Seasons List (core-v2)**

## Usage

``` r
espn_mch_seasons(limit = 200, ...)

espn_wch_seasons(limit = 200, ...)
```

## Arguments

- limit:

  Maximum number of seasons to return (default `200`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per season:

|            |           |                                            |
|------------|-----------|--------------------------------------------|
| col_name   | type      | description                                |
| ref        | character | `$ref` URL for the season object.          |
| season     | integer   | Season year parsed from the `$ref` URL.    |
| count      | integer   | Total number of seasons in the collection. |
| page_count | integer   | Total number of pages in the collection.   |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_seasons()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_seasons.md);
columns verified against a captured MCH body.

`espn_wch_seasons()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_seasons(limit = 10))
#> ── MENS-COLLEGE-HOCKEY Seasons List data from ESPN core-v2 ─────────────────────
#> ℹ Data updated: 2026-11-09 19:34:10 UTC
#> # A tibble: 10 × 4
#>    ref                                                   season count page_count
#>    <chr>                                                  <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2027    25          3
#>  2 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2026    25          3
#>  3 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2025    25          3
#>  4 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2024    25          3
#>  5 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2023    25          3
#>  6 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2022    25          3
#>  7 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2021    25          3
#>  8 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2020    25          3
#>  9 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2019    25          3
#> 10 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2018    25          3
# }
# \donttest{
  try(espn_wch_seasons(limit = 10))
#> ── WOMENS-COLLEGE-HOCKEY Seasons List data from ESPN core-v2 ───────────────────
#> ℹ Data updated: 2026-11-09 19:34:10 UTC
#> # A tibble: 10 × 4
#>    ref                                                   season count page_count
#>    <chr>                                                  <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2027    21          3
#>  2 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2026    21          3
#>  3 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2025    21          3
#>  4 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2024    21          3
#>  5 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2023    21          3
#>  6 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2022    21          3
#>  7 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2021    21          3
#>  8 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2020    21          3
#>  9 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2019    21          3
#> 10 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2018    21          3
# }
```
