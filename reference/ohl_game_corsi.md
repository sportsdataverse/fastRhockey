# **OHL Game Corsi/Fenwick (player-level on-ice)**

Player-level on-ice Corsi and Fenwick for an OHL game.

## Usage

``` r
ohl_game_corsi(game_id)
```

## Arguments

- game_id:

  Numeric OHL game identifier.

## Value

A `fastRhockey_data` data frame, one row per player.

## See also

Other OHL Functions:
[`most_recent_ohl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_ohl_season.md),
[`ohl`](https://fastRhockey.sportsdataverse.org/reference/ohl.md),
[`ohl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_shifts.md),
[`ohl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_summary.md),
[`ohl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ohl_leaders.md),
[`ohl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ohl_pbp.md),
[`ohl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/ohl_player_stats.md),
[`ohl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/ohl_player_toi.md),
[`ohl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ohl_schedule.md),
[`ohl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ohl_season_id.md),
[`ohl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ohl_standings.md),
[`ohl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ohl_team_roster.md),
[`ohl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ohl_teams.md)

## Examples

``` r
 try(ohl_game_corsi(game_id = 27225)) 
#> ── OHL Game Corsi from HockeyTech ───────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:33:50 UTC
#> # A tibble: 0 × 10
#> # ℹ 10 variables: player_id <chr>, corsi_for <int>, corsi_against <int>,
#> #   corsi_for_pct <dbl>, fenwick_for <int>, fenwick_against <int>,
#> #   fenwick_for_pct <dbl>, corsi_includes_missed <lgl>, toi_seconds <dbl>,
#> #   corsi_for_per60 <dbl>
```
