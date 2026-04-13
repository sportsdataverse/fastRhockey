# **NHL Records - Skater Real-Time Stats by Season**

Returns season-by-season skater real-time stats (hits, giveaways,
takeaways, blocks, faceoffs, time on ice) from the NHL Records API
(`https://records.nhl.com/site/api/skater-real-time-stats-season`).

## Usage

``` r
nhl_records_skater_real_time_stats_season(
  cayenne_exp = NULL,
  limit = NULL,
  start = NULL
)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string.

- limit:

  Optional integer page size.

- start:

  Optional integer pagination offset.

## Value

A `fastRhockey_data` tibble of skater real-time season stats, or `NULL`
on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_skater_real_time_stats_season(limit = 5))
#> ── NHL Records Skater Real-Time Stats Season ────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:50 UTC
#> # A tibble: 5 × 25
#>      id active_player blocked_shots faceoff_win_pctg faceoffs_lost
#>   <int> <lgl>                 <int> <lgl>                    <int>
#> 1  8421 FALSE                    34 NA                           0
#> 2 34901 FALSE                    34 NA                           0
#> 3  9083 FALSE                     2 NA                           0
#> 4 45649 FALSE                     2 NA                           0
#> 5  8170 FALSE                    30 NA                         216
#> # ℹ 20 more variables: faceoffs_taken <int>, faceoffs_won <int>,
#> #   first_name <chr>, franchise_id <int>, game_type_id <int>,
#> #   games_in_schedule <int>, games_played <int>, giveaways <int>, hits <int>,
#> #   last_name <chr>, missed_shots <int>, player_id <int>, position_code <chr>,
#> #   rookie_flag <lgl>, season_id <int>, shifts <int>, takeaways <int>,
#> #   team_abbrevs <chr>, team_names <chr>, time_on_ice <int>
# }
```
