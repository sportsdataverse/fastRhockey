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
#> ℹ Data updated: 2026-05-29 16:15:41 UTC
#> # A tibble: 1 × 40
#>           id   season game_type game_date  start_time_utc     eastern_utc_offset
#>        <int>    <int>     <int> <chr>      <chr>              <chr>             
#> 1 2025030315 20252026         3 2026-05-29 2026-05-30T00:00:… -04:00            
#> # ℹ 34 more variables: venue_utc_offset <chr>, tv_broadcasts <list>,
#> #   game_state <chr>, game_schedule_state <chr>, game_center_link <chr>,
#> #   series_url <chr>, neutral_site <lgl>, venue_timezone <chr>,
#> #   tickets_link <chr>, tickets_link_fr <chr>, team_leaders <list>,
#> #   venue_default <chr>, away_team_id <int>, away_team_abbrev <chr>,
#> #   away_team_record <chr>, away_team_logo <chr>, away_team_odds <list>,
#> #   away_team_name_default <chr>, home_team_id <int>, home_team_abbrev <chr>, …
# }
```
