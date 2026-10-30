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
#> ── Fox Sports NHL league_leaders ───────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:11:08 UTC
#> # A tibble: 100 × 7
#>    players v2               gp    entity_id g     a     p    
#>    <chr>   <chr>            <chr> <chr>     <chr> <chr> <chr>
#>  1 1       O. Ekman-Larsson 15    3222      NA    NA    NA   
#>  2 2       S. Couturier     15    3364      NA    NA    NA   
#>  3 3       C. Sissons       15    3916      NA    NA    NA   
#>  4 4       M. Rielly        15    4183      NA    NA    NA   
#>  5 5       J. McCabe        15    4325      NA    NA    NA   
#>  6 6       C. Dvorak        15    4518      NA    NA    NA   
#>  7 7       D. Raddysh       15    4543      NA    NA    NA   
#>  8 8       N. Paul          15    4617      NA    NA    NA   
#>  9 9       T. Sanheim       15    4628      NA    NA    NA   
#> 10 10      W. Nylander      15    4633      NA    NA    NA   
#> # ℹ 90 more rows
```
