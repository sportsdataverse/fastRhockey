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
#> ✖ 2026-06-13 02:50:15.284788: OHL game Corsi for game_id 27225 unavailable! Error in `$<-.data.frame`(`*tmp*`, "corsi_for_per60", value = NA_real_): replacement has 1 row, data has 0
#> data frame with 0 columns and 0 rows
```
