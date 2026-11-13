# **Get ESPN MCH / WCH Franchises (core-v2)**

**Get ESPN MCH / WCH Franchises (core-v2)**

## Usage

``` r
espn_mch_franchises(limit = 200, ...)

espn_wch_franchises(limit = 200, ...)
```

## Arguments

- limit:

  Maximum number of franchises to return (default `200`). The MCH has 32
  active franchises; `100` returns all in one request.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per franchise reference:

|              |           |                                               |
|--------------|-----------|-----------------------------------------------|
| col_name     | types     | description                                   |
| ref          | character | `$ref` URL for the franchise object.          |
| franchise_id | character | ESPN franchise id parsed from the `$ref` URL. |
| count        | integer   | Total franchises in the collection.           |
| page_count   | integer   | Total pages in the collection.                |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_franchises()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_franchises.md)
(columns inherited); a collection may be empty.

`espn_wch_franchises()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_franchises())
#> ── MENS-COLLEGE-HOCKEY Franchises data from ESPN core-v2 ── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:26:04 UTC
#> # A tibble: 116 × 4
#>    ref                                             franchise_id count page_count
#>    <chr>                                           <chr>        <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hock… 1              116          1
#>  2 http://sports.core.api.espn.com/v2/sports/hock… 9              116          1
#>  3 http://sports.core.api.espn.com/v2/sports/hock… 12             116          1
#>  4 http://sports.core.api.espn.com/v2/sports/hock… 41             116          1
#>  5 http://sports.core.api.espn.com/v2/sports/hock… 43             116          1
#>  6 http://sports.core.api.espn.com/v2/sports/hock… 87             116          1
#>  7 http://sports.core.api.espn.com/v2/sports/hock… 103            116          1
#>  8 http://sports.core.api.espn.com/v2/sports/hock… 104            116          1
#>  9 http://sports.core.api.espn.com/v2/sports/hock… 107            116          1
#> 10 http://sports.core.api.espn.com/v2/sports/hock… 108            116          1
#> # ℹ 106 more rows
# }
# \donttest{
  try(espn_wch_franchises())
#> ── WOMENS-COLLEGE-HOCKEY Franchises data from ESPN core-v2 ─────────────────────
#> ℹ Data updated: 2026-11-13 21:26:04 UTC
#> # A tibble: 47 × 4
#>    ref                                             franchise_id count page_count
#>    <chr>                                           <chr>        <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hock… 41              47          1
#>  2 http://sports.core.api.espn.com/v2/sports/hock… 43              47          1
#>  3 http://sports.core.api.espn.com/v2/sports/hock… 103             47          1
#>  4 http://sports.core.api.espn.com/v2/sports/hock… 104             47          1
#>  5 http://sports.core.api.espn.com/v2/sports/hock… 107             47          1
#>  6 http://sports.core.api.espn.com/v2/sports/hock… 108             47          1
#>  7 http://sports.core.api.espn.com/v2/sports/hock… 111             47          1
#>  8 http://sports.core.api.espn.com/v2/sports/hock… 132             47          1
#>  9 http://sports.core.api.espn.com/v2/sports/hock… 134             47          1
#> 10 http://sports.core.api.espn.com/v2/sports/hock… 135             47          1
#> # ℹ 37 more rows
# }
```
