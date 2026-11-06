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
#> ℹ Data updated: 2026-11-06 07:49:50 UTC
#> # A tibble: 11 × 41
#>            id   season game_type game_date  start_time_utc    eastern_utc_offset
#>         <int>    <int>     <int> <chr>      <chr>             <chr>             
#>  1 2026020251 20262027         2 2026-11-05 2026-11-06T00:00… -05:00            
#>  2 2026020252 20262027         2 2026-11-05 2026-11-06T00:00… -05:00            
#>  3 2026020253 20262027         2 2026-11-05 2026-11-06T00:00… -05:00            
#>  4 2026020254 20262027         2 2026-11-05 2026-11-06T00:00… -05:00            
#>  5 2026020255 20262027         2 2026-11-05 2026-11-06T00:00… -05:00            
#>  6 2026020256 20262027         2 2026-11-05 2026-11-06T00:00… -05:00            
#>  7 2026020257 20262027         2 2026-11-05 2026-11-06T00:00… -05:00            
#>  8 2026020258 20262027         2 2026-11-05 2026-11-06T00:00… -05:00            
#>  9 2026020259 20262027         2 2026-11-05 2026-11-06T00:00… -05:00            
#> 10 2026020260 20262027         2 2026-11-05 2026-11-06T01:00… -05:00            
#> 11 2026020261 20262027         2 2026-11-05 2026-11-06T02:30… -05:00            
#> # ℹ 35 more variables: venue_utc_offset <chr>, tv_broadcasts <list>,
#> #   game_state <chr>, game_schedule_state <chr>, game_center_link <chr>,
#> #   three_min_recap <chr>, three_min_recap_fr <chr>, condensed_game <chr>,
#> #   condensed_game_fr <chr>, neutral_site <lgl>, venue_timezone <chr>,
#> #   period <int>, goals <list>, venue_default <chr>, away_team_id <int>,
#> #   away_team_abbrev <chr>, away_team_score <int>, away_team_sog <int>,
#> #   away_team_logo <chr>, away_team_name_default <chr>, home_team_id <int>, …
# }
```
