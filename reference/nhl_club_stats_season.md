# **NHL Club Stats Season**

Returns season-by-season stats metadata for a club, including which game
types (regular season, playoffs) are available for each season.

## Usage

``` r
nhl_club_stats_season(team_abbr)
```

## Arguments

- team_abbr:

  Three-letter team abbreviation (e.g., "TOR", "BOS")

## Value

Returns a data frame with season/game-type availability.

## Examples

``` r
# \donttest{
  try(nhl_club_stats_season(team_abbr = "TOR"))
#> ── NHL Club Stats Season ────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 13:55:25 UTC
#> # A tibble: 98 × 3
#>      season game_types team_abbr
#>       <int> <list>     <chr>    
#>  1 20252026 <int [1]>  TOR      
#>  2 20242025 <int [2]>  TOR      
#>  3 20232024 <int [2]>  TOR      
#>  4 20222023 <int [2]>  TOR      
#>  5 20212022 <int [2]>  TOR      
#>  6 20202021 <int [2]>  TOR      
#>  7 20192020 <int [2]>  TOR      
#>  8 20182019 <int [2]>  TOR      
#>  9 20172018 <int [2]>  TOR      
#> 10 20162017 <int [2]>  TOR      
#> # ℹ 88 more rows
# }
```
