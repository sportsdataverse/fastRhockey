# **Get ESPN MCH / WCH Venues (core-v2)**

**Get ESPN MCH / WCH Venues (core-v2)**

## Usage

``` r
espn_mch_venues(limit = 1000, ...)

espn_wch_venues(limit = 1000, ...)
```

## Arguments

- limit:

  Maximum number of venues to return (default `1000`). The MCH venue
  catalog contains ~180 entries; `200` returns all in one request.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per venue reference:

|            |           |                                           |
|------------|-----------|-------------------------------------------|
| col_name   | type      | description                               |
| ref        | character | `$ref` URL for the venue object.          |
| venue_id   | character | ESPN venue id parsed from the `$ref` URL. |
| count      | integer   | Total venues in the collection.           |
| page_count | integer   | Total pages in the collection.            |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_venues()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_venues.md);
columns verified against a captured MCH body.

`espn_wch_venues()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_venues())
#> ── MENS-COLLEGE-HOCKEY Venues data from ESPN core-v2 ────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:26:47 UTC
#> # A tibble: 132 × 4
#>    ref                                                 venue_id count page_count
#>    <chr>                                               <chr>    <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hockey/l… 14         132          1
#>  2 http://sports.core.api.espn.com/v2/sports/hockey/l… 20         132          1
#>  3 http://sports.core.api.espn.com/v2/sports/hockey/l… 64         132          1
#>  4 http://sports.core.api.espn.com/v2/sports/hockey/l… 117        132          1
#>  5 http://sports.core.api.espn.com/v2/sports/hockey/l… 135        132          1
#>  6 http://sports.core.api.espn.com/v2/sports/hockey/l… 137        132          1
#>  7 http://sports.core.api.espn.com/v2/sports/hockey/l… 138        132          1
#>  8 http://sports.core.api.espn.com/v2/sports/hockey/l… 146        132          1
#>  9 http://sports.core.api.espn.com/v2/sports/hockey/l… 148        132          1
#> 10 http://sports.core.api.espn.com/v2/sports/hockey/l… 316        132          1
#> # ℹ 122 more rows
# }
# \donttest{
  try(espn_wch_venues())
#> ── WOMENS-COLLEGE-HOCKEY Venues data from ESPN core-v2 ──── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:26:47 UTC
#> # A tibble: 57 × 4
#>    ref                                                 venue_id count page_count
#>    <chr>                                               <chr>    <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hockey/l… 14          57          1
#>  2 http://sports.core.api.espn.com/v2/sports/hockey/l… 20          57          1
#>  3 http://sports.core.api.espn.com/v2/sports/hockey/l… 135         57          1
#>  4 http://sports.core.api.espn.com/v2/sports/hockey/l… 137         57          1
#>  5 http://sports.core.api.espn.com/v2/sports/hockey/l… 759         57          1
#>  6 http://sports.core.api.espn.com/v2/sports/hockey/l… 1204        57          1
#>  7 http://sports.core.api.espn.com/v2/sports/hockey/l… 1205        57          1
#>  8 http://sports.core.api.espn.com/v2/sports/hockey/l… 1207        57          1
#>  9 http://sports.core.api.espn.com/v2/sports/hockey/l… 1208        57          1
#> 10 http://sports.core.api.espn.com/v2/sports/hockey/l… 1209        57          1
#> # ℹ 47 more rows
# }
```
