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
#> ℹ Data updated: 2026-11-04 09:35:27 UTC
#> # A tibble: 11 × 41
#>            id   season game_type game_date  start_time_utc    eastern_utc_offset
#>         <int>    <int>     <int> <chr>      <chr>             <chr>             
#>  1 2026020236 20262027         2 2026-11-03 2026-11-04T00:00… -05:00            
#>  2 2026020237 20262027         2 2026-11-03 2026-11-04T00:00… -05:00            
#>  3 2026020238 20262027         2 2026-11-03 2026-11-04T00:00… -05:00            
#>  4 2026020240 20262027         2 2026-11-03 2026-11-04T00:00… -05:00            
#>  5 2026020241 20262027         2 2026-11-03 2026-11-04T00:00… -05:00            
#>  6 2026020242 20262027         2 2026-11-03 2026-11-04T00:00… -05:00            
#>  7 2026020243 20262027         2 2026-11-03 2026-11-04T00:00… -05:00            
#>  8 2026020244 20262027         2 2026-11-03 2026-11-04T00:00… -05:00            
#>  9 2026020239 20262027         2 2026-11-03 2026-11-04T00:30… -05:00            
#> 10 2026020245 20262027         2 2026-11-03 2026-11-04T01:00… -05:00            
#> 11 2026020246 20262027         2 2026-11-03 2026-11-04T02:00… -05:00            
#> # ℹ 35 more variables: venue_utc_offset <chr>, tv_broadcasts <list>,
#> #   game_state <chr>, game_schedule_state <chr>, game_center_link <chr>,
#> #   three_min_recap <chr>, three_min_recap_fr <chr>, condensed_game <chr>,
#> #   neutral_site <lgl>, venue_timezone <chr>, period <int>, goals <list>,
#> #   condensed_game_fr <chr>, venue_default <chr>, away_team_id <int>,
#> #   away_team_abbrev <chr>, away_team_score <int>, away_team_sog <int>,
#> #   away_team_logo <chr>, away_team_name_default <chr>, home_team_id <int>, …
# }
```
