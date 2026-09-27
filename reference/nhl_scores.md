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
#> ── NHL Scores ───────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-09-27 04:37:47 UTC
#> # A tibble: 14 × 41
#>            id   season game_type game_date  start_time_utc    eastern_utc_offset
#>         <int>    <int>     <int> <chr>      <chr>             <chr>             
#>  1 2026010053 20262027         1 2026-09-26 2026-09-26T19:00… -04:00            
#>  2 2026010055 20262027         1 2026-09-26 2026-09-26T19:00… -04:00            
#>  3 2026010052 20262027         1 2026-09-26 2026-09-26T20:00… -04:00            
#>  4 2026010054 20262027         1 2026-09-26 2026-09-26T21:00… -04:00            
#>  5 2026010057 20262027         1 2026-09-26 2026-09-26T21:00… -04:00            
#>  6 2026010059 20262027         1 2026-09-26 2026-09-26T22:00… -04:00            
#>  7 2026010056 20262027         1 2026-09-26 2026-09-26T23:00… -04:00            
#>  8 2026010058 20262027         1 2026-09-26 2026-09-26T23:00… -04:00            
#>  9 2026010061 20262027         1 2026-09-26 2026-09-26T23:00… -04:00            
#> 10 2026010064 20262027         1 2026-09-26 2026-09-26T23:00… -04:00            
#> 11 2026010065 20262027         1 2026-09-26 2026-09-26T23:00… -04:00            
#> 12 2026010062 20262027         1 2026-09-26 2026-09-26T23:30… -04:00            
#> 13 2026010060 20262027         1 2026-09-26 2026-09-27T01:00… -04:00            
#> 14 2026010063 20262027         1 2026-09-26 2026-09-27T02:00… -04:00            
#> # ℹ 35 more variables: venue_utc_offset <chr>, tv_broadcasts <list>,
#> #   game_state <chr>, game_schedule_state <chr>, game_center_link <chr>,
#> #   three_min_recap <chr>, three_min_recap_fr <chr>, condensed_game <chr>,
#> #   neutral_site <lgl>, venue_timezone <chr>, period <int>, goals <list>,
#> #   venue_default <chr>, away_team_id <int>, away_team_abbrev <chr>,
#> #   away_team_score <int>, away_team_sog <int>, away_team_logo <chr>,
#> #   away_team_name_default <chr>, away_team_name_fr <chr>, …
# }
```
