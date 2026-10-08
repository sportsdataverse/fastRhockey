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
#> ℹ Data updated: 2026-10-08 08:20:27 UTC
#> # A tibble: 3 × 40
#>           id   season game_type game_date  start_time_utc     eastern_utc_offset
#>        <int>    <int>     <int> <chr>      <chr>              <chr>             
#> 1 2026020053 20262027         2 2026-10-07 2026-10-07T23:30:… -04:00            
#> 2 2026020054 20262027         2 2026-10-07 2026-10-07T23:30:… -04:00            
#> 3 2026020055 20262027         2 2026-10-07 2026-10-08T02:00:… -04:00            
#> # ℹ 34 more variables: venue_utc_offset <chr>, tv_broadcasts <list>,
#> #   game_state <chr>, game_schedule_state <chr>, game_center_link <chr>,
#> #   three_min_recap <chr>, three_min_recap_fr <chr>, condensed_game <chr>,
#> #   neutral_site <lgl>, venue_timezone <chr>, period <int>, goals <list>,
#> #   condensed_game_fr <chr>, venue_default <chr>, away_team_id <int>,
#> #   away_team_abbrev <chr>, away_team_score <int>, away_team_sog <int>,
#> #   away_team_logo <chr>, away_team_name_default <chr>, home_team_id <int>, …
# }
```
