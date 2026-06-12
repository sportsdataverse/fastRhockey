# **AHL Standings**

AHL standings from the HockeyTech feed (one row per team).

## Usage

``` r
ahl_standings(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per team.

## See also

Other AHL Functions:
[`ahl`](https://fastRhockey.sportsdataverse.org/reference/ahl.md),
[`ahl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_corsi.md),
[`ahl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_shifts.md),
[`ahl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_summary.md),
[`ahl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ahl_leaders.md),
[`ahl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ahl_pbp.md),
[`ahl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/ahl_player_stats.md),
[`ahl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/ahl_player_toi.md),
[`ahl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ahl_schedule.md),
[`ahl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ahl_season_id.md),
[`ahl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ahl_team_roster.md),
[`ahl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ahl_teams.md),
[`most_recent_ahl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_ahl_season.md)

## Examples

``` r
 try(ahl_standings()) 
#> ── AHL Standings from HockeyTech ────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 13:19:20 UTC
#> # A tibble: 4 × 19
#>   team_code wins  losses ot_losses shootout_losses regulation_wins row   points
#>   <chr>     <chr>  <dbl> <chr>     <chr>                     <dbl> <chr>  <dbl>
#> 1 PAC       3          1 0         0                             3 3          6
#> 2 ATL       1          1 0         0                             1 1          3
#> 3 CEN       1          2 0         0                             1 1          3
#> 4 NOR       1          2 0         0                             1 1          2
#> # ℹ 11 more variables: penalty_minutes <chr>, streak <chr>, goals_for <chr>,
#> #   goals_against <chr>, games_remaining <chr>, percentage <chr>,
#> #   overall_rank <chr>, games_played <dbl>, team_rank <int>, past_10 <chr>,
#> #   team <chr>
```
