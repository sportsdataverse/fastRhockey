# **Get ESPN MCH / WCH Season Futures (core-v2)**

**Get ESPN MCH / WCH Season Futures (core-v2)**

## Usage

``` r
espn_mch_season_futures(season = most_recent_nhl_season(), ...)

espn_wch_season_futures(season = most_recent_nhl_season(), ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per futures market per betting
provider. For a typical MCH season there are ~12 markets across 1-2
providers:

|  |  |  |
|----|----|----|
| col_name | types | description |
| future_ref | character | `$ref` URL for the futures market object. |
| future_id | character | ESPN futures market identifier. |
| future_name | character | Futures market name (e.g. "MCH - Stanley Cup - Winner"). |
| future_type | character | Futures market type (e.g. "winLeague"). |
| future_display_name | character | Futures market display name. |
| provider_id | character | Betting provider identifier. |
| provider_name | character | Betting provider name (e.g. "ESPN BET"). |
| provider_active | logical | Whether the provider is active. |
| provider_priority | integer | Provider display priority. |
| season | integer | Season year (echoed from arg). |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_futures()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_futures.md)
(columns inherited); a collection may be empty.

`espn_wch_season_futures()` is the same wrapper for women's college
hockey (league slug `womens-college-hockey`): same arguments, same
columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_season_futures(season = 2026))
#> ── MENS-COLLEGE-HOCKEY Season Futures data from ESPN core-v2 ───────────────────
#> ℹ Data updated: 2026-11-09 11:49:27 UTC
#> # A tibble: 1 × 10
#>   future_ref   future_id future_name future_type future_display_name provider_id
#>   <chr>        <chr>     <chr>       <chr>       <chr>               <chr>      
#> 1 http://spor… 1600      NCAAH Cham… NA          NCAAH Championship… 58         
#> # ℹ 4 more variables: provider_name <chr>, provider_active <lgl>,
#> #   provider_priority <int>, season <int>
# }
# \donttest{
  try(espn_wch_season_futures(season = 2026))
#> data frame with 0 columns and 0 rows
# }
```
