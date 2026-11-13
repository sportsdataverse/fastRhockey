# **Get ESPN NHL Franchises (core-v2)**

**Get ESPN NHL Franchises (core-v2)**

## Usage

``` r
espn_nhl_franchises(limit = 100, ...)
```

## Arguments

- limit:

  Maximum number of franchises to return (default `100`). The NHL has 32
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_franchises())
#> ── NHL Franchises data from ESPN core-v2 ────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:16:27 UTC
#> # A tibble: 32 × 4
#>    ref                                             franchise_id count page_count
#>    <chr>                                           <chr>        <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hock… 1               32          1
#>  2 http://sports.core.api.espn.com/v2/sports/hock… 2               32          1
#>  3 http://sports.core.api.espn.com/v2/sports/hock… 3               32          1
#>  4 http://sports.core.api.espn.com/v2/sports/hock… 4               32          1
#>  5 http://sports.core.api.espn.com/v2/sports/hock… 5               32          1
#>  6 http://sports.core.api.espn.com/v2/sports/hock… 6               32          1
#>  7 http://sports.core.api.espn.com/v2/sports/hock… 7               32          1
#>  8 http://sports.core.api.espn.com/v2/sports/hock… 8               32          1
#>  9 http://sports.core.api.espn.com/v2/sports/hock… 9               32          1
#> 10 http://sports.core.api.espn.com/v2/sports/hock… 10              32          1
#> # ℹ 22 more rows
# }
```
