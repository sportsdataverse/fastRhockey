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
#> ℹ Data updated: 2026-11-25 20:19:01 UTC
#> # A tibble: 100 × 7
#>    players v2           gp    entity_id g     a     p    
#>    <chr>   <chr>        <chr> <chr>     <chr> <chr> <chr>
#>  1 1       C. Murphy    27    4062      NA    NA    NA   
#>  2 2       L. Draisaitl 27    4393      NA    NA    NA   
#>  3 3       K. Kapanen   27    4405      NA    NA    NA   
#>  4 4       C. McDavid   27    4743      NA    NA    NA   
#>  5 5       M. Joseph    27    4943      NA    NA    NA   
#>  6 6       A. Formenton 27    5726      NA    NA    NA   
#>  7 7       E. Bouchard  27    5877      NA    NA    NA   
#>  8 8       R. Shea      27    6592      NA    NA    NA   
#>  9 9       V. Podkolzin 27    7049      NA    NA    NA   
#> 10 10      O. Michaels  27    8760      NA    NA    NA   
#> # ℹ 90 more rows
```
