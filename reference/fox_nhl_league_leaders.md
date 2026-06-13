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
#> ℹ Data updated: 2026-06-13 07:18:35 UTC
#> # A tibble: 100 × 7
#>    players v2           gp    entity_id g     a     p    
#>    <chr>   <chr>        <chr> <chr>     <chr> <chr> <chr>
#>  1 1       S. Theodore  21    3534      NA    NA    NA   
#>  2 2       C. Sissons   21    3916      NA    NA    NA   
#>  3 3       T. Hertl     21    4109      NA    NA    NA   
#>  4 4       N. Dowd      21    4328      NA    NA    NA   
#>  5 5       I. Barbashev 21    4412      NA    NA    NA   
#>  6 6       J. Eichel    21    4741      NA    NA    NA   
#>  7 7       N. Hanifin   21    4742      NA    NA    NA   
#>  8 8       K. Kolesar   21    4849      NA    NA    NA   
#>  9 9       M. Marner    21    4908      NA    NA    NA   
#> 10 10      R. Andersson 21    4913      NA    NA    NA   
#> # ℹ 90 more rows
```
