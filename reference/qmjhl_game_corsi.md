# **QMJHL Game Corsi/Fenwick (player-level on-ice)**

Player-level on-ice Corsi and Fenwick for a QMJHL game.

## Usage

``` r
qmjhl_game_corsi(game_id)
```

## Arguments

- game_id:

  Numeric QMJHL game identifier.

## Value

A `fastRhockey_data` data frame, one row per player.

## See also

Other QMJHL Functions:
[`most_recent_qmjhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_qmjhl_season.md),
[`qmjhl`](https://fastRhockey.sportsdataverse.org/reference/qmjhl.md),
[`qmjhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_shifts.md),
[`qmjhl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_summary.md),
[`qmjhl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_leaders.md),
[`qmjhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_pbp.md),
[`qmjhl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_stats.md),
[`qmjhl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_toi.md),
[`qmjhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_schedule.md),
[`qmjhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_season_id.md),
[`qmjhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_standings.md),
[`qmjhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_team_roster.md),
[`qmjhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_teams.md)

## Examples

``` r
 try(qmjhl_game_corsi(game_id = 27225)) 
#> ── QMJHL Game Corsi from HockeyTech ─────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:53:41 UTC
#> # A tibble: 0 × 10
#> # ℹ 10 variables: player_id <chr>, corsi_for <int>, corsi_against <int>,
#> #   corsi_for_pct <dbl>, fenwick_for <int>, fenwick_against <int>,
#> #   fenwick_for_pct <dbl>, corsi_includes_missed <lgl>, toi_seconds <dbl>,
#> #   corsi_for_per60 <dbl>
```
