# **AHL Game Corsi/Fenwick (player-level on-ice)**

Player-level on-ice Corsi and Fenwick for an AHL game.

## Usage

``` r
ahl_game_corsi(game_id)
```

## Arguments

- game_id:

  Numeric AHL game identifier.

## Value

A `fastRhockey_data` data frame, one row per player.

## See also

Other AHL Functions:
[`ahl`](https://fastRhockey.sportsdataverse.org/reference/ahl.md),
[`ahl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_shifts.md),
[`ahl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_summary.md),
[`ahl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ahl_leaders.md),
[`ahl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ahl_pbp.md),
[`ahl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/ahl_player_stats.md),
[`ahl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/ahl_player_toi.md),
[`ahl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ahl_schedule.md),
[`ahl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ahl_season_id.md),
[`ahl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ahl_standings.md),
[`ahl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ahl_team_roster.md),
[`ahl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ahl_teams.md),
[`most_recent_ahl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_ahl_season.md)

## Examples

``` r
 try(ahl_game_corsi(game_id = 1000093924)) 
#> ── AHL Game Corsi from HockeyTech ───────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:17:33 UTC
#> # A tibble: 0 × 10
#> # ℹ 10 variables: player_id <chr>, corsi_for <int>, corsi_against <int>,
#> #   corsi_for_pct <dbl>, fenwick_for <int>, fenwick_against <int>,
#> #   fenwick_for_pct <dbl>, corsi_includes_missed <lgl>, toi_seconds <dbl>,
#> #   corsi_for_per60 <dbl>
```
