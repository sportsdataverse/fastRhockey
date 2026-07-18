# **WHL Player Time On Ice**

Per-player time-on-ice totals for a WHL game.

## Usage

``` r
whl_player_toi(game_id)
```

## Arguments

- game_id:

  Numeric WHL game identifier.

## Value

A `fastRhockey_data` data frame, one row per player.

## See also

Other WHL Functions:
[`most_recent_whl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_whl_season.md),
[`whl`](https://fastRhockey.sportsdataverse.org/reference/whl.md),
[`whl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_corsi.md),
[`whl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_shifts.md),
[`whl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_summary.md),
[`whl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/whl_leaders.md),
[`whl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/whl_pbp.md),
[`whl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/whl_player_stats.md),
[`whl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/whl_schedule.md),
[`whl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/whl_season_id.md),
[`whl_standings()`](https://fastRhockey.sportsdataverse.org/reference/whl_standings.md),
[`whl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/whl_team_roster.md),
[`whl_teams()`](https://fastRhockey.sportsdataverse.org/reference/whl_teams.md)

## Examples

``` r
 try(whl_player_toi(game_id = 27225)) 
#> ── WHL Player TOI from HockeyTech ───────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-18 17:05:43 UTC
#> # A tibble: 0 × 6
#> # ℹ 6 variables: player_id <int>, first_name <chr>, last_name <chr>,
#> #   toi_seconds <int>, num_shifts <int>, avg_shift_s <dbl>
```
