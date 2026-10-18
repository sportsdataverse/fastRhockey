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
#> ℹ Data updated: 2026-10-18 05:45:24 UTC
#> # A tibble: 100 × 7
#>    players v2             gp    entity_id g     a     p    
#>    <chr>   <chr>          <chr> <chr>     <chr> <chr> <chr>
#>  1 1       M. Zibanejad   10    3362      NA    NA    NA   
#>  2 2       S. Couturier   10    3364      NA    NA    NA   
#>  3 3       J. Miller      10    3475      NA    NA    NA   
#>  4 4       O. Bjorkstrand 10    3685      NA    NA    NA   
#>  5 5       C. Dvorak      10    4518      NA    NA    NA   
#>  6 6       T. Sanheim     10    4628      NA    NA    NA   
#>  7 7       N. Acciari     10    4733      NA    NA    NA   
#>  8 8       M. Pettersson  10    4736      NA    NA    NA   
#>  9 9       N. Seeler      10    5040      NA    NA    NA   
#> 10 10      F. Hronek      10    5276      NA    NA    NA   
#> # ℹ 90 more rows
```
