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
#> ℹ Data updated: 2026-11-06 07:46:41 UTC
#> # A tibble: 100 × 7
#>    players v2              gp    entity_id g     a     p    
#>    <chr>   <chr>           <chr> <chr>     <chr> <chr> <chr>
#>  1 1       B. McNabb       18    3271      NA    NA    NA   
#>  2 2       M. Stone        18    3361      NA    NA    NA   
#>  3 3       S. Couturier    18    3364      NA    NA    NA   
#>  4 4       S. Theodore     18    3534      NA    NA    NA   
#>  5 5       J. Martinook    18    4060      NA    NA    NA   
#>  6 6       T. Hertl        18    4109      NA    NA    NA   
#>  7 7       W. Carrier      18    4125      NA    NA    NA   
#>  8 8       N. Dowd         18    4328      NA    NA    NA   
#>  9 9       S. Gostisbehere 18    4346      NA    NA    NA   
#> 10 10      I. Barbashev    18    4412      NA    NA    NA   
#> # ℹ 90 more rows
```
