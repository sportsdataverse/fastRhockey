# **Get ESPN NHL Providers (core-v2)**

**Get ESPN NHL Providers (core-v2)**

## Usage

``` r
espn_nhl_providers(limit = 100, ...)
```

## Arguments

- limit:

  Maximum number of providers to return (default `100`). The NHL
  provider catalog contains ~33 entries; `100` returns all in one
  request.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per odds provider reference:

|             |           |                                              |
|-------------|-----------|----------------------------------------------|
| col_name    | types     | description                                  |
| ref         | character | `$ref` URL for the provider object.          |
| provider_id | character | ESPN provider id parsed from the `$ref` URL. |
| count       | integer   | Total providers in the collection.           |
| page_count  | integer   | Total pages in the collection.               |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_providers())
#> ── NHL Providers data from ESPN core-v2 ─────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:27:18 UTC
#> # A tibble: 31 × 4
#>    ref                                              provider_id count page_count
#>    <chr>                                            <chr>       <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hocke… 25             31          1
#>  2 http://sports.core.api.espn.com/v2/sports/hocke… 28             31          1
#>  3 http://sports.core.api.espn.com/v2/sports/hocke… 31             31          1
#>  4 http://sports.core.api.espn.com/v2/sports/hocke… 32             31          1
#>  5 http://sports.core.api.espn.com/v2/sports/hocke… 35             31          1
#>  6 http://sports.core.api.espn.com/v2/sports/hocke… 36             31          1
#>  7 http://sports.core.api.espn.com/v2/sports/hocke… 37             31          1
#>  8 http://sports.core.api.espn.com/v2/sports/hocke… 38             31          1
#>  9 http://sports.core.api.espn.com/v2/sports/hocke… 39             31          1
#> 10 http://sports.core.api.espn.com/v2/sports/hocke… 40             31          1
#> # ℹ 21 more rows
# }
```
