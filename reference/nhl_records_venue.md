# **NHL Records - Venue Listing**

Returns the NHL arena/venue listing from the NHL Records API
(`https://records.nhl.com/site/api/venue`).

## Usage

``` r
nhl_records_venue(cayenne_exp = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string.

## Value

A `fastRhockey_data` tibble of venues, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_venue())
#> ── NHL Records Venue ────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:51 UTC
#> # A tibble: 618 × 10
#>       id building_id city    country_code outdoor postal_code short_country_code
#>    <int>       <int> <chr>   <chr>        <lgl>   <chr>       <chr>             
#>  1     1          94 Uniond… USA          FALSE   NA          US                
#>  2     2         364 Yarmou… CAN          FALSE   B5A 4R3     CA                
#>  3     3           5 Dallas  USA          FALSE   75219       US                
#>  4     4         264 Abbots… CAN          FALSE   V2S 8H8     CA                
#>  5     5          47 Detroit USA          FALSE   NA          US                
#>  6     6         132 Prague  CZE          FALSE   NA          CZ                
#>  7     7         122 Chicago USA          TRUE    NA          US                
#>  8     8          53 Salzbu… AUT          FALSE   NA          AT                
#>  9     9         375 Hamilt… CAN          FALSE   L9A 2N3     CA                
#> 10    10         318 Pentic… CAN          FALSE   V2A 9C4     CA                
#> # ℹ 608 more rows
#> # ℹ 3 more variables: state_province_code <chr>, venue_id <int>,
#> #   venue_name <chr>
# }
```
