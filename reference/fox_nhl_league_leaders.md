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
#> ── Fox Sports NHL league_leaders ────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-11 16:01:36 UTC
#> # A tibble: 100 × 7
#>    players v2               gp    entity_id g     a     p    
#>    <chr>   <chr>            <chr> <chr>     <chr> <chr> <chr>
#>  1 1       S. Crosby        6     2460      NA    NA    NA   
#>  2 2       K. Letang        6     2634      NA    NA    NA   
#>  3 3       E. Malkin        6     2635      NA    NA    NA   
#>  4 4       J. Staal         6     2636      NA    NA    NA   
#>  5 5       P. Kane          6     2687      NA    NA    NA   
#>  6 6       E. Karlsson      6     3068      NA    NA    NA   
#>  7 7       T. Hall          6     3156      NA    NA    NA   
#>  8 8       O. Ekman-Larsson 6     3222      NA    NA    NA   
#>  9 9       I. Cole          6     3233      NA    NA    NA   
#> 10 10      C. Tanev         6     3253      NA    NA    NA   
#> # ℹ 90 more rows
```
