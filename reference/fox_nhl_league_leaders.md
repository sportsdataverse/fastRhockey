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
#> ℹ Data updated: 2026-10-12 12:06:47 UTC
#> # A tibble: 100 × 7
#>    players v2              gp    entity_id g     a     p    
#>    <chr>   <chr>           <chr> <chr>     <chr> <chr> <chr>
#>  1 1       T. Hall         7     3156      NA    NA    NA   
#>  2 2       M. Zibanejad    7     3362      NA    NA    NA   
#>  3 3       S. Couturier    7     3364      NA    NA    NA   
#>  4 4       J. Miller       7     3475      NA    NA    NA   
#>  5 5       O. Bjorkstrand  7     3685      NA    NA    NA   
#>  6 6       J. Martinook    7     4060      NA    NA    NA   
#>  7 7       W. Carrier      7     4125      NA    NA    NA   
#>  8 8       S. Gostisbehere 7     4346      NA    NA    NA   
#>  9 9       N. Ehlers       7     4441      NA    NA    NA   
#> 10 10      C. Dvorak       7     4518      NA    NA    NA   
#> # ℹ 90 more rows
```
