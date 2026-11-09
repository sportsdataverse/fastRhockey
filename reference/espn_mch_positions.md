# **Get ESPN MCH / WCH Positions (core-v2)**

**Get ESPN MCH / WCH Positions (core-v2)**

## Usage

``` r
espn_mch_positions(limit = 200, ...)

espn_wch_positions(limit = 200, ...)
```

## Arguments

- limit:

  Maximum number of positions to return (default `200`). The MCH
  position catalog contains ~28 entries (positions + sub-positions).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per position reference:

|             |           |                                              |
|-------------|-----------|----------------------------------------------|
| col_name    | types     | description                                  |
| ref         | character | `$ref` URL for the position object.          |
| position_id | character | ESPN position id parsed from the `$ref` URL. |
| count       | integer   | Total positions in the collection.           |
| page_count  | integer   | Total pages in the collection.               |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_positions()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_positions.md)
(columns inherited); a collection may be empty.

`espn_wch_positions()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_positions())
#> ── MENS-COLLEGE-HOCKEY Positions data from ESPN core-v2 ─── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:18:37 UTC
#> # A tibble: 28 × 4
#>    ref                                              position_id count page_count
#>    <chr>                                            <chr>       <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hocke… 1              28          1
#>  2 http://sports.core.api.espn.com/v2/sports/hocke… 2              28          1
#>  3 http://sports.core.api.espn.com/v2/sports/hocke… 3              28          1
#>  4 http://sports.core.api.espn.com/v2/sports/hocke… 4              28          1
#>  5 http://sports.core.api.espn.com/v2/sports/hocke… 5              28          1
#>  6 http://sports.core.api.espn.com/v2/sports/hocke… 8              28          1
#>  7 http://sports.core.api.espn.com/v2/sports/hocke… 9              28          1
#>  8 http://sports.core.api.espn.com/v2/sports/hocke… 11             28          1
#>  9 http://sports.core.api.espn.com/v2/sports/hocke… 12             28          1
#> 10 http://sports.core.api.espn.com/v2/sports/hocke… 13             28          1
#> # ℹ 18 more rows
# }
# \donttest{
  try(espn_wch_positions())
#> ── WOMENS-COLLEGE-HOCKEY Positions data from ESPN core-v2 ──────────────────────
#> ℹ Data updated: 2026-11-09 17:18:37 UTC
#> # A tibble: 28 × 4
#>    ref                                              position_id count page_count
#>    <chr>                                            <chr>       <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hocke… 1              28          1
#>  2 http://sports.core.api.espn.com/v2/sports/hocke… 2              28          1
#>  3 http://sports.core.api.espn.com/v2/sports/hocke… 3              28          1
#>  4 http://sports.core.api.espn.com/v2/sports/hocke… 4              28          1
#>  5 http://sports.core.api.espn.com/v2/sports/hocke… 5              28          1
#>  6 http://sports.core.api.espn.com/v2/sports/hocke… 8              28          1
#>  7 http://sports.core.api.espn.com/v2/sports/hocke… 9              28          1
#>  8 http://sports.core.api.espn.com/v2/sports/hocke… 11             28          1
#>  9 http://sports.core.api.espn.com/v2/sports/hocke… 12             28          1
#> 10 http://sports.core.api.espn.com/v2/sports/hocke… 13             28          1
#> # ℹ 18 more rows
# }
```
