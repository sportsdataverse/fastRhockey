# **NHL Stats API — Franchise Listing**

Returns the franchise listing from the NHL Stats REST API
(`https://api.nhle.com/stats/rest/{lang}/franchise`). Parallels the
`nhl-api-py` `Teams.franchises()` helper.

## Usage

``` r
nhl_stats_franchise(lang = "en", limit = 100, start = 0, cayenne_exp = NULL)
```

## Arguments

- lang:

  Character language code. Default `"en"`.

- limit:

  Integer maximum number of results. Default 100.

- start:

  Integer pagination start index. Default 0.

- cayenne_exp:

  Optional Cayenne filter expression string passed via the `cayenneExp`
  query parameter (e.g., `"id=1"`).

## Value

A `fastRhockey_data` tibble of franchises, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_stats_franchise())
#> ── NHL Stats Franchise ──────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:59 UTC
#> # A tibble: 40 × 4
#>       id full_name             team_common_name team_place_name
#>    <int> <chr>                 <chr>            <chr>          
#>  1    32 Anaheim Ducks         Ducks            Anaheim        
#>  2     8 Brooklyn Americans    Americans        Brooklyn       
#>  3    16 Philadelphia Flyers   Flyers           Philadelphia   
#>  4     7 Montreal Maroons      Maroons          Montreal       
#>  5     2 Montreal Wanderers    Wanderers        Montreal       
#>  6    36 Columbus Blue Jackets Blue Jackets     Columbus       
#>  7    11 Chicago Blackhawks    Blackhawks       Chicago        
#>  8    33 Florida Panthers      Panthers         Florida        
#>  9    28 Arizona Coyotes       Coyotes          Arizona        
#> 10    29 San Jose Sharks       Sharks           San Jose       
#> # ℹ 30 more rows
# }
```
