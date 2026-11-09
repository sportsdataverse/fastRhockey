# **Get ESPN NHL Season Futures (core-v2)**

**Get ESPN NHL Season Futures (core-v2)**

## Usage

``` r
espn_nhl_season_futures(season = most_recent_nhl_season(), ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per futures market per betting
provider. For a typical NHL season there are ~12 markets across 1-2
providers:

|  |  |  |
|----|----|----|
| col_name | types | description |
| future_ref | character | `$ref` URL for the futures market object. |
| future_id | character | ESPN futures market identifier. |
| future_name | character | Futures market name (e.g. "NHL - Stanley Cup - Winner"). |
| future_type | character | Futures market type (e.g. "winLeague"). |
| future_display_name | character | Futures market display name. |
| provider_id | character | Betting provider identifier. |
| provider_name | character | Betting provider name (e.g. "ESPN BET"). |
| provider_active | logical | Whether the provider is active. |
| provider_priority | integer | Provider display priority. |
| season | integer | Season year (echoed from arg). |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_season_futures(season = 2026))
#> ── NHL Season Futures data from ESPN core-v2 ────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:56 UTC
#> # A tibble: 11 × 10
#>    future_ref  future_id future_name future_type future_display_name provider_id
#>    <chr>       <chr>     <chr>       <chr>       <chr>               <chr>      
#>  1 http://spo… 2118      NHL - Stan… winLeague   Stanley Cup Winner  58         
#>  2 http://spo… 14492     NHL - West… winConfere… Western Conference… 58         
#>  3 http://spo… 14493     NHL - East… winConfere… Eastern Conference… 58         
#>  4 http://spo… 14494     NHL - Rock… NA          NHL - Rocket Richa… 58         
#>  5 http://spo… 14495     NHL - Hart… NA          NHL - Hart Trophy … 58         
#>  6 http://spo… 14496     NHL - Art … NA          NHL - Art Ross Tro… 58         
#>  7 http://spo… 15719     NHL - Atla… NA          Atlantic Division … 58         
#>  8 http://spo… 15722     NHL - Cent… NA          Central Division W… 58         
#>  9 http://spo… 15724     NHL - Paci… NA          Pacific Division W… 58         
#> 10 http://spo… 74045     NHL - Metr… NA          Metropolitan Divis… 58         
#> 11 http://spo… 100126    NHL - Pres… NA          Presidents Trophy   58         
#> # ℹ 4 more variables: provider_name <chr>, provider_active <lgl>,
#> #   provider_priority <int>, season <int>
# }
```
