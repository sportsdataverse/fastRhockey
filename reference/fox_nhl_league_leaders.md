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
#> ℹ Data updated: 2026-10-08 12:57:37 UTC
#> # A tibble: 100 × 7
#>    players v2             gp    entity_id g     a     p    
#>    <chr>   <chr>          <chr> <chr>     <chr> <chr> <chr>
#>  1 1       M. Zibanejad   5     3362      NA    NA    NA   
#>  2 2       J. Miller      5     3475      NA    NA    NA   
#>  3 3       O. Bjorkstrand 5     3685      NA    NA    NA   
#>  4 4       M. Pettersson  5     4736      NA    NA    NA   
#>  5 5       E. Tolvanen    5     5807      NA    NA    NA   
#>  6 6       S. Durzi       5     5922      NA    NA    NA   
#>  7 7       V. Gavrikov    5     6027      NA    NA    NA   
#>  8 8       A. Fox         5     6037      NA    NA    NA   
#>  9 9       P. Dorofeyev   5     6127      NA    NA    NA   
#> 10 10      A. Lafreniere  5     6365      NA    NA    NA   
#> # ℹ 90 more rows
```
