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
#> ℹ Data updated: 2026-04-08 07:40:21 UTC
#> # A tibble: 11 × 42
#>            id   season game_type game_date  start_time_utc    eastern_utc_offset
#>         <int>    <int>     <int> <chr>      <chr>             <chr>             
#>  1 2025021233 20252026         2 2026-04-07 2026-04-07T23:00… -04:00            
#>  2 2025021234 20252026         2 2026-04-07 2026-04-07T23:00… -04:00            
#>  3 2025021235 20252026         2 2026-04-07 2026-04-07T23:00… -04:00            
#>  4 2025021236 20252026         2 2026-04-07 2026-04-07T23:00… -04:00            
#>  5 2025021237 20252026         2 2026-04-07 2026-04-07T23:00… -04:00            
#>  6 2025021238 20252026         2 2026-04-07 2026-04-08T00:00… -04:00            
#>  7 2025021239 20252026         2 2026-04-07 2026-04-08T00:00… -04:00            
#>  8 2025021240 20252026         2 2026-04-07 2026-04-08T00:00… -04:00            
#>  9 2025021241 20252026         2 2026-04-07 2026-04-08T01:30… -04:00            
#> 10 2025021242 20252026         2 2026-04-07 2026-04-08T02:00… -04:00            
#> 11 2025021243 20252026         2 2026-04-07 2026-04-08T02:00… -04:00            
#> # ℹ 36 more variables: venue_utc_offset <chr>, tv_broadcasts <list>,
#> #   game_state <chr>, game_schedule_state <chr>, game_center_link <chr>,
#> #   three_min_recap <chr>, three_min_recap_fr <chr>, condensed_game <chr>,
#> #   condensed_game_fr <chr>, neutral_site <lgl>, venue_timezone <chr>,
#> #   period <int>, goals <list>, venue_default <chr>, away_team_id <int>,
#> #   away_team_abbrev <chr>, away_team_score <int>, away_team_sog <int>,
#> #   away_team_logo <chr>, away_team_name_default <chr>, home_team_id <int>, …
# }
```
