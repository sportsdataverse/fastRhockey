# **Get ESPN NHL Awards (core-v2)**

**Get ESPN NHL Awards (core-v2)**

## Usage

``` r
espn_nhl_awards(limit = 100, ...)
```

## Arguments

- limit:

  Maximum number of awards to return per page (default `100`). The NHL
  has 34 awards; `100` returns all in one request.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per award:

|            |           |                                           |
|------------|-----------|-------------------------------------------|
| col_name   | types     | description                               |
| ref        | character | `$ref` URL for the award object.          |
| award_id   | character | ESPN award id parsed from the `$ref` URL. |
| count      | integer   | Total awards in the collection.           |
| page_count | integer   | Total pages in the collection.            |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_awards())
#> ── NHL Awards data from ESPN core-v2 ────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:41 UTC
#> # A tibble: 34 × 4
#>    ref                                                 award_id count page_count
#>    <chr>                                               <chr>    <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hockey/l… 1           34          1
#>  2 http://sports.core.api.espn.com/v2/sports/hockey/l… 2           34          1
#>  3 http://sports.core.api.espn.com/v2/sports/hockey/l… 3           34          1
#>  4 http://sports.core.api.espn.com/v2/sports/hockey/l… 4           34          1
#>  5 http://sports.core.api.espn.com/v2/sports/hockey/l… 5           34          1
#>  6 http://sports.core.api.espn.com/v2/sports/hockey/l… 9           34          1
#>  7 http://sports.core.api.espn.com/v2/sports/hockey/l… 20          34          1
#>  8 http://sports.core.api.espn.com/v2/sports/hockey/l… 21          34          1
#>  9 http://sports.core.api.espn.com/v2/sports/hockey/l… 22          34          1
#> 10 http://sports.core.api.espn.com/v2/sports/hockey/l… 24          34          1
#> # ℹ 24 more rows
# }
```
