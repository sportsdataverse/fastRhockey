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
#> ℹ Data updated: 2026-11-02 13:00:37 UTC
#> # A tibble: 5 × 41
#>           id   season game_type game_date  start_time_utc     eastern_utc_offset
#>        <int>    <int>     <int> <chr>      <chr>              <chr>             
#> 1 2026020227 20262027         2 2026-11-01 2026-11-01T18:00:… -05:00            
#> 2 2026020228 20262027         2 2026-11-01 2026-11-01T20:30:… -05:00            
#> 3 2026020229 20262027         2 2026-11-01 2026-11-01T21:00:… -05:00            
#> 4 2026020230 20262027         2 2026-11-01 2026-11-01T23:00:… -05:00            
#> 5 2026020231 20262027         2 2026-11-01 2026-11-02T01:00:… -05:00            
#> # ℹ 35 more variables: venue_utc_offset <chr>, tv_broadcasts <list>,
#> #   game_state <chr>, game_schedule_state <chr>, game_center_link <chr>,
#> #   three_min_recap <chr>, three_min_recap_fr <chr>, condensed_game <chr>,
#> #   condensed_game_fr <chr>, neutral_site <lgl>, venue_timezone <chr>,
#> #   period <int>, goals <list>, venue_default <chr>, away_team_id <int>,
#> #   away_team_abbrev <chr>, away_team_score <int>, away_team_sog <int>,
#> #   away_team_logo <chr>, away_team_name_default <chr>, …
# }
```
