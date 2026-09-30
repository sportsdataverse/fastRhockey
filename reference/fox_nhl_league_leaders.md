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
#> ℹ Data updated: 2026-09-30 14:43:01 UTC
#> # A tibble: 100 × 7
#>    players v2               gp    entity_id g     a     p    
#>    <chr>   <chr>            <chr> <chr>     <chr> <chr> <chr>
#>  1 1       J. Staal         1     2636      NA    NA    NA   
#>  2 2       P. Kane          1     2687      NA    NA    NA   
#>  3 3       L. Schenn        1     2959      NA    NA    NA   
#>  4 4       D. Kulikov       1     3016      NA    NA    NA   
#>  5 5       J. Tavares       1     3057      NA    NA    NA   
#>  6 6       L. Eller         1     3089      NA    NA    NA   
#>  7 7       T. Hall          1     3156      NA    NA    NA   
#>  8 8       J. Markstrom     1     3163      NA    NA    NA   
#>  9 9       S. Bobrovsky     1     3217      NA    NA    NA   
#> 10 10      O. Ekman-Larsson 1     3222      NA    NA    NA   
#> # ℹ 90 more rows
```
