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
#> ℹ Data updated: 2026-11-02 14:11:14 UTC
#> # A tibble: 100 × 7
#>    players v2               gp    entity_id g     a     p    
#>    <chr>   <chr>            <chr> <chr>     <chr> <chr> <chr>
#>  1 1       C. Giroux        16    2762      NA    NA    NA   
#>  2 2       O. Ekman-Larsson 16    3222      NA    NA    NA   
#>  3 3       S. Couturier     16    3364      NA    NA    NA   
#>  4 4       J. Brodin        16    3449      NA    NA    NA   
#>  5 5       R. Hartman       16    3642      NA    NA    NA   
#>  6 6       M. McCarron      16    3883      NA    NA    NA   
#>  7 7       C. Sissons       16    3916      NA    NA    NA   
#>  8 8       N. Cousins       16    4023      NA    NA    NA   
#>  9 9       J. Martinook     16    4060      NA    NA    NA   
#> 10 10      W. Carrier       16    4125      NA    NA    NA   
#> # ℹ 90 more rows
```
