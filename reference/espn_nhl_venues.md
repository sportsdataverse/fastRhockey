# **Get ESPN NHL Venues (core-v2)**

**Get ESPN NHL Venues (core-v2)**

## Usage

``` r
espn_nhl_venues(limit = 200, ...)
```

## Arguments

- limit:

  Maximum number of venues to return (default `200`). The NHL venue
  catalog contains ~180 entries; `200` returns all in one request.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per venue reference:

|            |           |                                           |
|------------|-----------|-------------------------------------------|
| col_name   | types     | description                               |
| ref        | character | `$ref` URL for the venue object.          |
| venue_id   | character | ESPN venue id parsed from the `$ref` URL. |
| count      | integer   | Total venues in the collection.           |
| page_count | integer   | Total pages in the collection.            |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_venues())
#> ── NHL Venues data from ESPN core-v2 ────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:17:16 UTC
#> # A tibble: 185 × 4
#>    ref                                                 venue_id count page_count
#>    <chr>                                               <chr>    <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hockey/l… 1          185          1
#>  2 http://sports.core.api.espn.com/v2/sports/hockey/l… 4          185          1
#>  3 http://sports.core.api.espn.com/v2/sports/hockey/l… 8          185          1
#>  4 http://sports.core.api.espn.com/v2/sports/hockey/l… 9          185          1
#>  5 http://sports.core.api.espn.com/v2/sports/hockey/l… 10         185          1
#>  6 http://sports.core.api.espn.com/v2/sports/hockey/l… 14         185          1
#>  7 http://sports.core.api.espn.com/v2/sports/hockey/l… 19         185          1
#>  8 http://sports.core.api.espn.com/v2/sports/hockey/l… 20         185          1
#>  9 http://sports.core.api.espn.com/v2/sports/hockey/l… 21         185          1
#> 10 http://sports.core.api.espn.com/v2/sports/hockey/l… 22         185          1
#> # ℹ 175 more rows
# }
```
