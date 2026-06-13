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
#> ── Fox Sports NHL odds ──────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-13 04:19:48 UTC
#> # A tibble: 0 × 0
```
