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
#> ── NHL Scores ──────────────────────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:16:57 UTC
#> # A tibble: 14 × 42
#>            id   season game_type game_date  start_time_utc    eastern_utc_offset
#>         <int>    <int>     <int> <chr>      <chr>             <chr>             
#>  1 2026020121 20262027         2 2026-10-17 2026-10-17T17:00… -04:00            
#>  2 2026020122 20262027         2 2026-10-17 2026-10-17T19:30… -04:00            
#>  3 2026020123 20262027         2 2026-10-17 2026-10-17T20:00… -04:00            
#>  4 2026020124 20262027         2 2026-10-17 2026-10-17T22:00… -04:00            
#>  5 2026020125 20262027         2 2026-10-17 2026-10-17T23:00… -04:00            
#>  6 2026020126 20262027         2 2026-10-17 2026-10-17T23:00… -04:00            
#>  7 2026020127 20262027         2 2026-10-17 2026-10-17T23:00… -04:00            
#>  8 2026020128 20262027         2 2026-10-17 2026-10-17T23:00… -04:00            
#>  9 2026020129 20262027         2 2026-10-17 2026-10-17T23:00… -04:00            
#> 10 2026020130 20262027         2 2026-10-17 2026-10-17T23:00… -04:00            
#> 11 2026020131 20262027         2 2026-10-17 2026-10-18T00:00… -04:00            
#> 12 2026020132 20262027         2 2026-10-17 2026-10-18T01:00… -04:00            
#> 13 2026020133 20262027         2 2026-10-17 2026-10-18T02:00… -04:00            
#> 14 2026020134 20262027         2 2026-10-17 2026-10-18T02:00… -04:00            
#> # ℹ 36 more variables: venue_utc_offset <chr>, tv_broadcasts <list>,
#> #   game_state <chr>, game_schedule_state <chr>, game_center_link <chr>,
#> #   three_min_recap <chr>, three_min_recap_fr <chr>, condensed_game <chr>,
#> #   neutral_site <lgl>, venue_timezone <chr>, period <int>, goals <list>,
#> #   condensed_game_fr <chr>, venue_default <chr>, away_team_id <int>,
#> #   away_team_abbrev <chr>, away_team_score <int>, away_team_sog <int>,
#> #   away_team_logo <chr>, away_team_name_default <chr>, …
# }
```
