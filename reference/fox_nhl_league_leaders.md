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
#> ℹ Data updated: 2026-11-24 19:32:54 UTC
#> # A tibble: 100 × 7
#>    players v2           gp    entity_id g     a     p    
#>    <chr>   <chr>        <chr> <chr>     <chr> <chr> <chr>
#>  1 1       S. Couturier 26    3364      NA    NA    NA   
#>  2 2       D. Orlov     26    3393      NA    NA    NA   
#>  3 3       T. Toffoli   26    3448      NA    NA    NA   
#>  4 4       J. Brodin    26    3449      NA    NA    NA   
#>  5 5       R. Hartman   26    3642      NA    NA    NA   
#>  6 6       B. Goodrow   26    3744      NA    NA    NA   
#>  7 7       C. Murphy    26    4062      NA    NA    NA   
#>  8 8       J. McCabe    26    4325      NA    NA    NA   
#>  9 9       L. Draisaitl 26    4393      NA    NA    NA   
#> 10 10      K. Kapanen   26    4405      NA    NA    NA   
#> # ℹ 90 more rows
```
