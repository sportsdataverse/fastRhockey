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
#> ℹ Data updated: 2026-11-01 21:57:04 UTC
#> # A tibble: 100 × 7
#>    players v2               gp    entity_id g     a     p    
#>    <chr>   <chr>            <chr> <chr>     <chr> <chr> <chr>
#>  1 1       O. Ekman-Larsson 16    3222      NA    NA    NA   
#>  2 2       C. Sissons       16    3916      NA    NA    NA   
#>  3 3       J. Martinook     16    4060      NA    NA    NA   
#>  4 4       W. Carrier       16    4125      NA    NA    NA   
#>  5 5       M. Rielly        16    4183      NA    NA    NA   
#>  6 6       J. McCabe        16    4325      NA    NA    NA   
#>  7 7       S. Gostisbehere  16    4346      NA    NA    NA   
#>  8 8       N. Ehlers        16    4441      NA    NA    NA   
#>  9 9       D. Raddysh       16    4543      NA    NA    NA   
#> 10 10      N. Paul          16    4617      NA    NA    NA   
#> # ℹ 90 more rows
```
