# **Get Fox Sports NHL game odds**

**Get Fox Sports NHL game odds**

## Usage

``` r
fox_nhl_odds(game_id)
```

## Arguments

- game_id:

  Fox Bifrost event id.

## Value

A `fastRhockey_data` tibble, one row per team (`game_id`, `team`, plus
six-pack odds columns). Empty when no market is posted.

## Examples

``` r
 try(fox_nhl_odds("44398")) 
#> ── Fox Sports NHL odds ──────────────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:27:58 UTC
#> # A tibble: 2 × 5
#>   game_id team           puck_line to_win total
#>   <chr>   <chr>          <chr>     <chr>  <chr>
#> 1 44398   Boston Bruins  +1.5      +140   O 6.5
#> 2 44398   Buffalo Sabres -1.5      -166   U 6.5
```
