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
#> ℹ Data updated: 2026-11-09 22:51:31 UTC
#> # A tibble: 2 × 10
#>   future_ref   future_id future_name future_type future_display_name provider_id
#>   <chr>        <chr>     <chr>       <chr>       <chr>               <chr>      
#> 1 http://spor… 2118      NHL - Stan… winLeague   Stanley Cup Winner  58         
#> 2 http://spor… 14495     NHL - Hart… NA          NHL - Hart Trophy … 58         
#> # ℹ 4 more variables: provider_name <chr>, provider_active <lgl>,
#> #   provider_priority <int>, season <int>
# }
```
