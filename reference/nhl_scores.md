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
#> ℹ Data updated: 2026-10-11 16:04:03 UTC
#> # A tibble: 3 × 29
#>           id   season game_type game_date  start_time_utc     eastern_utc_offset
#>        <int>    <int>     <int> <chr>      <chr>              <chr>             
#> 1 2026020084 20262027         2 2026-10-11 2026-10-11T21:00:… -04:00            
#> 2 2026020085 20262027         2 2026-10-11 2026-10-11T22:00:… -04:00            
#> 3 2026020086 20262027         2 2026-10-11 2026-10-11T23:00:… -04:00            
#> # ℹ 23 more variables: venue_utc_offset <chr>, tv_broadcasts <list>,
#> #   game_state <chr>, game_schedule_state <chr>, game_center_link <chr>,
#> #   neutral_site <lgl>, venue_timezone <chr>, tickets_link <chr>,
#> #   tickets_link_fr <chr>, team_leaders <list>, venue_default <chr>,
#> #   away_team_id <int>, away_team_abbrev <chr>, away_team_record <chr>,
#> #   away_team_logo <chr>, away_team_odds <list>, away_team_name_default <chr>,
#> #   home_team_id <int>, home_team_abbrev <chr>, home_team_record <chr>, …
# }
```
