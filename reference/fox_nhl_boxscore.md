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
#> ── Fox Sports NHL boxscore ──────────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-25 20:19:01 UTC
#> # A tibble: 0 × 0
```
