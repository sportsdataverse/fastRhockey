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
#> ℹ Data updated: 2026-10-09 05:35:15 UTC
#> # A tibble: 100 × 7
#>    players v2           gp    entity_id g     a     p    
#>    <chr>   <chr>        <chr> <chr>     <chr> <chr> <chr>
#>  1 1       J. Staal     5     2636      NA    NA    NA   
#>  2 2       P. Kane      5     2687      NA    NA    NA   
#>  3 3       T. Hall      5     3156      NA    NA    NA   
#>  4 4       I. Cole      5     3233      NA    NA    NA   
#>  5 5       M. Zibanejad 5     3362      NA    NA    NA   
#>  6 6       S. Couturier 5     3364      NA    NA    NA   
#>  7 7       J. Oleksiak  5     3430      NA    NA    NA   
#>  8 8       A. Lee       5     3473      NA    NA    NA   
#>  9 9       J. Miller    5     3475      NA    NA    NA   
#> 10 10      H. Lindholm  5     3528      NA    NA    NA   
#> # ℹ 90 more rows
```
