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
#> ℹ Data updated: 2026-11-13 21:27:57 UTC
#> # A tibble: 100 × 7
#>    players v2               gp    entity_id g     a     p    
#>    <chr>   <chr>            <chr> <chr>     <chr> <chr> <chr>
#>  1 1       O. Ekman-Larsson 21    3222      NA    NA    NA   
#>  2 2       S. Couturier     21    3364      NA    NA    NA   
#>  3 3       D. Orlov         21    3393      NA    NA    NA   
#>  4 4       T. Toffoli       21    3448      NA    NA    NA   
#>  5 5       J. Brodin        21    3449      NA    NA    NA   
#>  6 6       R. Hartman       21    3642      NA    NA    NA   
#>  7 7       B. Goodrow       21    3744      NA    NA    NA   
#>  8 8       J. Trouba        21    4277      NA    NA    NA   
#>  9 9       J. McCabe        21    4325      NA    NA    NA   
#> 10 10      C. Dvorak        21    4518      NA    NA    NA   
#> # ℹ 90 more rows
```
