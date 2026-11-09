# **Get ESPN NHL Positions (core-v2)**

**Get ESPN NHL Positions (core-v2)**

## Usage

``` r
espn_nhl_positions(limit = 100, ...)
```

## Arguments

- limit:

  Maximum number of positions to return (default `100`). The NHL
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_positions())
#> ── NHL Positions data from ESPN core-v2 ─────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:50:02 UTC
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
