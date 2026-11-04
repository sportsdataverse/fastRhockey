# **AHL Play-by-Play**

AHL play-by-play for a single game (one row per event, fully enriched
with coordinate transforms, on-ice players, Corsi geometry).

## Usage

``` r
ahl_pbp(game_id)
```

## Arguments

- game_id:

  Numeric AHL game identifier.

## Value

A `fastRhockey_data` data frame, one row per play-by-play event.

## See also

Other AHL Functions:
[`ahl`](https://fastRhockey.sportsdataverse.org/reference/ahl.md),
[`ahl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_corsi.md),
[`ahl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_shifts.md),
[`ahl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_summary.md),
[`ahl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ahl_leaders.md),
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
 try(ahl_pbp(game_id = 1000093924)) 
#> ── AHL Play-by-Play from HockeyTech ─────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-04 04:35:47 UTC
#> # A tibble: 0 × 32
#> # ℹ 32 variables: game_date <chr>, game_season <int>, game_season_id <chr>,
#> #   home_team <chr>, home_team_id <chr>, away_team <chr>, away_team_id <chr>,
#> #   x_coord_original <dbl>, y_coord_original <dbl>, x_coord_neutral <dbl>,
#> #   y_coord_neutral <dbl>, x_coord_fixed <dbl>, y_coord_fixed <dbl>,
#> #   x_coord_right <dbl>, y_coord_right <dbl>, x_coord_vertical <dbl>,
#> #   y_coord_vertical <dbl>, minute_start <int>, second_start <int>,
#> #   clock <chr>, sec_from_start <int>, short_handed <chr>, power_play <chr>, …
```
