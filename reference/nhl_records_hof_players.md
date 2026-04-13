# **NHL Records - Hall of Fame Players**

Returns Hall of Fame inductees from the NHL Records API
(`https://records.nhl.com/site/api/hof/players`). Optionally filter by
office ID (switches resource to `hof/players/{office_id}`).

## Usage

``` r
nhl_records_hof_players(office_id = NULL)
```

## Arguments

- office_id:

  Optional integer office/category ID. If supplied, the resource becomes
  `hof/players/{office_id}`.

## Value

A `fastRhockey_data` tibble of HOF inductees, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_hof_players())
#> ── NHL Records HOF Players ──────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:40 UTC
#> # A tibble: 761 × 7
#>       id date_inducted     induction_cat_id misc_full_name office_id official_id
#>    <int> <chr>                        <int> <chr>              <int> <lgl>      
#>  1     1 1949-01-01T00:00…                1 "Donald H. \"…         1 NA         
#>  2     2 1945-01-01T00:00…                1 "HobartBaker"          1 NA         
#>  3     3 1952-01-01T00:00…                1 "Richard R. \…         1 NA         
#>  4     4 1947-01-01T00:00…                1 "RussellBowie"         1 NA         
#>  5     5 1950-01-01T00:00…                1 "Alan M. \"Sc…         1 NA         
#>  6     6 1950-01-01T00:00…                1 "Charles Grah…         1 NA         
#>  7     7 1974-01-01T00:00…                1 "ThomasDunder…         1 NA         
#>  8     8 1962-01-01T00:00…                1 "Hamilton Liv…         1 NA         
#>  9     9 1952-01-01T00:00…                1 "Frank Xavier…         1 NA         
#> 10    10 2010-01-01T00:00…                1 "CammiGranato"         1 NA         
#> # ℹ 751 more rows
#> # ℹ 1 more variable: player_id <int>
# }
```
