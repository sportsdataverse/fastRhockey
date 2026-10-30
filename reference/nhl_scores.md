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
#> ℹ Data updated: 2026-10-30 00:18:44 UTC
#> # A tibble: 11 × 51
#>            id   season game_type game_date  start_time_utc    eastern_utc_offset
#>         <int>    <int>     <int> <chr>      <chr>             <chr>             
#>  1 2026020204 20262027         2 2026-10-29 2026-10-29T23:00… -04:00            
#>  2 2026020205 20262027         2 2026-10-29 2026-10-29T23:00… -04:00            
#>  3 2026020206 20262027         2 2026-10-29 2026-10-29T23:00… -04:00            
#>  4 2026020203 20262027         2 2026-10-29 2026-10-29T23:30… -04:00            
#>  5 2026020207 20262027         2 2026-10-29 2026-10-30T00:00… -04:00            
#>  6 2026020208 20262027         2 2026-10-29 2026-10-30T00:00… -04:00            
#>  7 2026020209 20262027         2 2026-10-29 2026-10-30T00:00… -04:00            
#>  8 2026020210 20262027         2 2026-10-29 2026-10-30T00:00… -04:00            
#>  9 2026020211 20262027         2 2026-10-29 2026-10-30T02:00… -04:00            
#> 10 2026020212 20262027         2 2026-10-29 2026-10-30T02:00… -04:00            
#> 11 2026020213 20262027         2 2026-10-29 2026-10-30T02:00… -04:00            
#> # ℹ 45 more variables: venue_utc_offset <chr>, tv_broadcasts <list>,
#> #   game_state <chr>, game_schedule_state <chr>, game_center_link <chr>,
#> #   neutral_site <lgl>, venue_timezone <chr>, period <int>, goals <list>,
#> #   tickets_link <chr>, tickets_link_fr <chr>, team_leaders <list>,
#> #   venue_default <chr>, away_team_id <int>, away_team_abbrev <chr>,
#> #   away_team_score <int>, away_team_sog <int>, away_team_logo <chr>,
#> #   away_team_record <chr>, away_team_odds <list>, …
# }
```
