# **NHL Records - Skater Real-Time Stats Career**

Returns career skater real-time stats (hits, giveaways, takeaways,
blocks, faceoffs, time on ice) from the NHL Records API
(`https://records.nhl.com/site/api/skater-real-time-stats-career`).

## Usage

``` r
nhl_records_skater_real_time_stats_career(
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

A `fastRhockey_data` tibble of skater real-time career stats, or `NULL`
on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_skater_real_time_stats_career(limit = 5))
#> ── NHL Records Skater Real-Time Stats Career ────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:50 UTC
#> # A tibble: 5 × 24
#>      id active_player blocked_shots faceoff_win_pctg faceoffs_lost
#>   <int> <lgl>                 <int> <lgl>                    <int>
#> 1  4032 FALSE                    34 NA                           0
#> 2 11259 FALSE                    34 NA                           0
#> 3  4033 FALSE                     2 NA                           0
#> 4 15997 FALSE                     2 NA                           0
#> 5  4034 FALSE                    30 NA                         216
#> # ℹ 19 more variables: faceoffs_taken <int>, faceoffs_won <int>,
#> #   first_name <chr>, first_season_for_game_type <int>, franchise_id <int>,
#> #   game_type_id <int>, games_played <int>, giveaways <int>, hits <int>,
#> #   last_name <chr>, last_season_for_game_type <int>, missed_shots <int>,
#> #   player_id <int>, position_code <chr>, seasons_played <int>, shifts <int>,
#> #   takeaways <int>, team_abbrevs <chr>, time_on_ice <int>
# }
```
