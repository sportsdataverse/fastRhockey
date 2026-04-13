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
#> ℹ Data updated: 2026-04-13 17:05:55 UTC
#> # A tibble: 10 × 29
#>            id   season game_type game_date  start_time_utc    eastern_utc_offset
#>         <int>    <int>     <int> <chr>      <chr>             <chr>             
#>  1 2025021282 20252026         2 2026-04-13 2026-04-13T23:00… -04:00            
#>  2 2025021283 20252026         2 2026-04-13 2026-04-13T23:00… -04:00            
#>  3 2025021284 20252026         2 2026-04-13 2026-04-13T23:00… -04:00            
#>  4 2025021285 20252026         2 2026-04-13 2026-04-13T23:30… -04:00            
#>  5 2025021286 20252026         2 2026-04-13 2026-04-14T00:00… -04:00            
#>  6 2025021287 20252026         2 2026-04-13 2026-04-14T00:00… -04:00            
#>  7 2025021288 20252026         2 2026-04-13 2026-04-14T00:30… -04:00            
#>  8 2025021289 20252026         2 2026-04-13 2026-04-14T01:30… -04:00            
#>  9 2025021291 20252026         2 2026-04-13 2026-04-14T01:30… -04:00            
#> 10 2025021290 20252026         2 2026-04-13 2026-04-14T02:00… -04:00            
#> # ℹ 23 more variables: venue_utc_offset <chr>, tv_broadcasts <list>,
#> #   game_state <chr>, game_schedule_state <chr>, game_center_link <chr>,
#> #   neutral_site <lgl>, venue_timezone <chr>, tickets_link <chr>,
#> #   tickets_link_fr <chr>, team_leaders <list>, venue_default <chr>,
#> #   away_team_id <int>, away_team_abbrev <chr>, away_team_record <chr>,
#> #   away_team_logo <chr>, away_team_odds <list>, away_team_name_default <chr>,
#> #   home_team_id <int>, home_team_abbrev <chr>, home_team_record <chr>, …
# }
```
