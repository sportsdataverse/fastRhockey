# **Get Fox Sports NHL statistical leaders**

**Get Fox Sports NHL statistical leaders**

## Usage

``` r
fox_nhl_league_leaders(category = "scoring", who = "player", page = 0)
```

## Arguments

- category:

  Stat category (default `"scoring"`).

- who:

  `"player"` or `"team"` (default `"player"`).

- page:

  0-based page index (default `0`).

## Value

A `fastRhockey_data` tibble of leaderboard rows (`entity_id` + stat
columns).

## Examples

``` r
 try(fox_nhl_league_leaders("scoring")) 
#> ── Fox Sports NHL league_leaders ────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:50:18 UTC
#> # A tibble: 100 × 7
#>    players v2               gp    entity_id g     a     p    
#>    <chr>   <chr>            <chr> <chr>     <chr> <chr> <chr>
#>  1 1       O. Ekman-Larsson 19    3222      NA    NA    NA   
#>  2 2       B. McNabb        19    3271      NA    NA    NA   
#>  3 3       M. Stone         19    3361      NA    NA    NA   
#>  4 4       S. Couturier     19    3364      NA    NA    NA   
#>  5 5       D. Orlov         19    3393      NA    NA    NA   
#>  6 6       T. Toffoli       19    3448      NA    NA    NA   
#>  7 7       J. Brodin        19    3449      NA    NA    NA   
#>  8 8       S. Theodore      19    3534      NA    NA    NA   
#>  9 9       R. Hartman       19    3642      NA    NA    NA   
#> 10 10      B. Goodrow       19    3744      NA    NA    NA   
#> # ℹ 90 more rows
```
