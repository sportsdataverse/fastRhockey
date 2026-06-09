# **AHL Player Stats**

AHL player season stats across all seasons.

## Usage

``` r
ahl_player_stats(player_id)
```

## Arguments

- player_id:

  Numeric or character AHL player identifier.

## Value

A `fastRhockey_data` data frame, one row per season-stat entry.

## See also

Other AHL Functions:
[`ahl`](https://fastRhockey.sportsdataverse.org/reference/ahl.md),
[`ahl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_corsi.md),
[`ahl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_shifts.md),
[`ahl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_summary.md),
[`ahl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ahl_leaders.md),
[`ahl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ahl_pbp.md),
[`ahl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/ahl_player_toi.md),
[`ahl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ahl_schedule.md),
[`ahl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ahl_season_id.md),
[`ahl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ahl_standings.md),
[`ahl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ahl_team_roster.md),
[`ahl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ahl_teams.md),
[`most_recent_ahl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_ahl_season.md)

## Examples

``` r
 try(ahl_player_stats(player_id = 10781)) 
#> ── AHL Player Stats from HockeyTech ─────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-09 20:25:30 UTC
#> # A tibble: 2 × 50
#>   season_id season_name            shortname playoff career sopt_track_faceoffs
#>   <chr>     <chr>                  <chr>     <chr>   <chr>  <chr>              
#> 1 90        2025-26 Regular Season 2025-26   0       1      0                  
#> 2 90        Total                  Total     0       1      NA                 
#> # ℹ 44 more variables: max_start_date <chr>, veteran_status <chr>,
#> #   veteran <chr>, jersey_number <chr>, goals <chr>, games_played <chr>,
#> #   assists <chr>, points <chr>, plus_minus <chr>, penalty_minutes <chr>,
#> #   power_play_goals <chr>, power_play_assists <chr>, shots <chr>,
#> #   shootout_attempts <chr>, shootout_goals <chr>, shootout_percentage <chr>,
#> #   shooting_percentage <chr>, shootout_winning_goals <chr>,
#> #   points_per_game <chr>, short_handed_goals <chr>, …
```
