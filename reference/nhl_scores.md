# **NHL Scores**

Returns scores for all games on a given date.

## Usage

``` r
nhl_scores(date = NULL)
```

## Arguments

- date:

  Character date in "YYYY-MM-DD" format. If NULL, returns current
  scores.

## Value

Returns a data frame with game scores.

## Examples

``` r
# \donttest{
  try(nhl_scores())
#> ── NHL Scores ───────────────────────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-25 20:23:53 UTC
#> # A tibble: 14 × 30
#>            id   season game_type game_date  start_time_utc    eastern_utc_offset
#>         <int>    <int>     <int> <chr>      <chr>             <chr>             
#>  1 2026020386 20262027         2 2026-11-25 2026-11-26T00:00… -05:00            
#>  2 2026020387 20262027         2 2026-11-25 2026-11-26T00:00… -05:00            
#>  3 2026020388 20262027         2 2026-11-25 2026-11-26T00:00… -05:00            
#>  4 2026020389 20262027         2 2026-11-25 2026-11-26T00:00… -05:00            
#>  5 2026020390 20262027         2 2026-11-25 2026-11-26T00:00… -05:00            
#>  6 2026020391 20262027         2 2026-11-25 2026-11-26T00:00… -05:00            
#>  7 2026020392 20262027         2 2026-11-25 2026-11-26T00:00… -05:00            
#>  8 2026020393 20262027         2 2026-11-25 2026-11-26T00:00… -05:00            
#>  9 2026020394 20262027         2 2026-11-25 2026-11-26T00:30… -05:00            
#> 10 2026020395 20262027         2 2026-11-25 2026-11-26T01:00… -05:00            
#> 11 2026020396 20262027         2 2026-11-25 2026-11-26T01:00… -05:00            
#> 12 2026020397 20262027         2 2026-11-25 2026-11-26T02:30… -05:00            
#> 13 2026020399 20262027         2 2026-11-25 2026-11-26T02:40… -05:00            
#> 14 2026020398 20262027         2 2026-11-25 2026-11-26T03:00… -05:00            
#> # ℹ 24 more variables: venue_utc_offset <chr>, tv_broadcasts <list>,
#> #   game_state <chr>, game_schedule_state <chr>, game_center_link <chr>,
#> #   neutral_site <lgl>, venue_timezone <chr>, tickets_link <chr>,
#> #   tickets_link_fr <chr>, team_leaders <list>, venue_default <chr>,
#> #   away_team_id <int>, away_team_abbrev <chr>, away_team_record <chr>,
#> #   away_team_logo <chr>, away_team_odds <list>, away_team_name_default <chr>,
#> #   away_team_name_fr <chr>, home_team_id <int>, home_team_abbrev <chr>, …
# }
```
