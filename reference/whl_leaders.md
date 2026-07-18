# **WHL Statistical Leaders**

WHL statistical leaders for a season from the HockeyTech feed.

## Usage

``` r
whl_leaders(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per player entry.

## See also

Other WHL Functions:
[`most_recent_whl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_whl_season.md),
[`whl`](https://fastRhockey.sportsdataverse.org/reference/whl.md),
[`whl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_corsi.md),
[`whl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_shifts.md),
[`whl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_summary.md),
[`whl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/whl_pbp.md),
[`whl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/whl_player_stats.md),
[`whl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/whl_player_toi.md),
[`whl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/whl_schedule.md),
[`whl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/whl_season_id.md),
[`whl_standings()`](https://fastRhockey.sportsdataverse.org/reference/whl_standings.md),
[`whl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/whl_team_roster.md),
[`whl_teams()`](https://fastRhockey.sportsdataverse.org/reference/whl_teams.md)

## Examples

``` r
 try(whl_leaders()) 
#> ── WHL Leaders from HockeyTech ──────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-18 17:05:42 UTC
#> # A tibble: 0 × 0
```
