# **Get Fox Sports NHL boxscore**

**Get Fox Sports NHL boxscore**

## Usage

``` r
fox_nhl_boxscore(game_id)
```

## Arguments

- game_id:

  Fox Bifrost event id.

## Value

A `fastRhockey_data` tibble (long), one row per (player, stat):
`game_id`, `team`, `stat_group`, `player`, `athlete_id`, `stat`,
`value`.

## Examples

``` r
 try(fox_nhl_boxscore("44398")) 
#> ── Fox Sports NHL boxscore ──────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-13 02:48:02 UTC
#> # A tibble: 438 × 7
#>    game_id team   stat_group player    athlete_id stat  value
#>    <chr>   <chr>  <chr>      <chr>     <chr>      <chr> <chr>
#>  1 44398   BRUINS SKATERS    C. McAvoy 5126       g     0    
#>  2 44398   BRUINS SKATERS    C. McAvoy 5126       a     1    
#>  3 44398   BRUINS SKATERS    C. McAvoy 5126       x     2    
#>  4 44398   BRUINS SKATERS    C. McAvoy 5126       s     1    
#>  5 44398   BRUINS SKATERS    C. McAvoy 5126       pim   4    
#>  6 44398   BRUINS SKATERS    C. McAvoy 5126       toi   27:22
#>  7 44398   BRUINS SKATERS    C. McAvoy 5126       h     4    
#>  8 44398   BRUINS SKATERS    C. McAvoy 5126       bs    6    
#>  9 44398   BRUINS SKATERS    C. McAvoy 5126       ta    0    
#> 10 44398   BRUINS SKATERS    C. McAvoy 5126       ga    4    
#> # ℹ 428 more rows
```
