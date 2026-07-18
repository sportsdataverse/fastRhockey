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
#> ℹ Data updated: 2026-07-18 17:02:40 UTC
#> # A tibble: 100 × 7
#>    players v2          gp    entity_id g     a     p    
#>    <chr>   <chr>       <chr> <chr>     <chr> <chr> <chr>
#>  1 1       B. Kulak    83    3603      NA    NA    NA   
#>  2 2       B. Burns    82    2225      NA    NA    NA   
#>  3 3       A. Ovechkin 82    2513      NA    NA    NA   
#>  4 4       J. Toews    82    2690      NA    NA    NA   
#>  5 5       C. Giroux   82    2762      NA    NA    NA   
#>  6 6       M. Backlund 82    2820      NA    NA    NA   
#>  7 7       S. Stamkos  82    2949      NA    NA    NA   
#>  8 8       J. Tavares  82    3057      NA    NA    NA   
#>  9 9       C. Fowler   82    3109      NA    NA    NA   
#> 10 10      I. Cole     82    3233      NA    NA    NA   
#> # ℹ 90 more rows
```
