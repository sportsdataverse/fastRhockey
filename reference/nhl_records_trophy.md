# **NHL Records - Trophy Listing**

Returns the NHL trophy listing from the NHL Records API
(`https://records.nhl.com/site/api/trophy`).

## Usage

``` r
nhl_records_trophy(cayenne_exp = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string.

## Value

A `fastRhockey_data` tibble of trophies, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_trophy())
#> ── NHL Records Trophy ───────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:51 UTC
#> # A tibble: 25 × 10
#>       id brief_description           category_id created_on description footnote
#>    <int> <chr>                             <int> <chr>      <chr>       <lgl>   
#>  1     1 NHL Champion                          1 2025-08-0… "<p><stron… NA      
#>  2     2 Coach of the Year                     3 2025-06-0… "<p><b>Des… NA      
#>  3     3 Sportsmanship and Gentlema…           2 2025-06-1… "<p><b>Des… NA      
#>  4     4 Rookie of the Year                    2 2025-06-1… "<p><b>Des… NA      
#>  5     5 Western Conference Champion           1 2025-05-2… "<p><b>Des… NA      
#>  6     6 Leadership and Humanitaria…           2 2025-06-0… "<p><b>Des… NA      
#>  7     7 MVP of Stanley Cup Playoffs           2 2025-06-2… "<p><b>Des… NA      
#>  8     8 MVP of Regular Season                 2 2025-06-1… "<p><b>Des… NA      
#>  9     9 Goaltender(s) on Team with…           2 2025-04-2… "<p><b>Des… NA      
#> 10    10 Perseverance, Sportsmanshi…           2 2025-06-0… "<p><b>Des… NA      
#> # ℹ 15 more rows
#> # ℹ 4 more variables: home_page_url <chr>, image_url <chr>, name <chr>,
#> #   short_name <chr>
# }
```
