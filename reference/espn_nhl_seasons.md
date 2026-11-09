# **Get ESPN NHL Seasons List (core-v2)**

**Get ESPN NHL Seasons List (core-v2)**

## Usage

``` r
espn_nhl_seasons(limit = 100, ...)
```

## Arguments

- limit:

  Maximum number of seasons to return (default `100`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per season:

|            |           |                                            |
|------------|-----------|--------------------------------------------|
| col_name   | types     | description                                |
| ref        | character | `$ref` URL for the season object.          |
| season     | integer   | Season year parsed from the `$ref` URL.    |
| count      | integer   | Total number of seasons in the collection. |
| page_count | integer   | Total number of pages in the collection.   |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_seasons(limit = 10))
#> ── NHL Seasons List data from ESPN core-v2 ──────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:35:00 UTC
#> # A tibble: 10 × 4
#>    ref                                                   season count page_count
#>    <chr>                                                  <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2027    67          7
#>  2 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2026    67          7
#>  3 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2025    67          7
#>  4 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2024    67          7
#>  5 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2023    67          7
#>  6 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2022    67          7
#>  7 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2021    67          7
#>  8 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2020    67          7
#>  9 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2019    67          7
#> 10 http://sports.core.api.espn.com/v2/sports/hockey/lea…   2018    67          7
# }
```
