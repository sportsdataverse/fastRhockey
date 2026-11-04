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
#> ℹ Data updated: 2026-11-04 09:32:01 UTC
#> # A tibble: 100 × 7
#>    players v2               gp    entity_id g     a     p    
#>    <chr>   <chr>            <chr> <chr>     <chr> <chr> <chr>
#>  1 1       O. Ekman-Larsson 17    3222      NA    NA    NA   
#>  2 2       B. McNabb        17    3271      NA    NA    NA   
#>  3 3       M. Stone         17    3361      NA    NA    NA   
#>  4 4       S. Couturier     17    3364      NA    NA    NA   
#>  5 5       J. Brodin        17    3449      NA    NA    NA   
#>  6 6       S. Theodore      17    3534      NA    NA    NA   
#>  7 7       R. Hartman       17    3642      NA    NA    NA   
#>  8 8       M. McCarron      17    3883      NA    NA    NA   
#>  9 9       C. Sissons       17    3916      NA    NA    NA   
#> 10 10      J. Martinook     17    4060      NA    NA    NA   
#> # ℹ 90 more rows
```
