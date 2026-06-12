# **WHL Game Corsi/Fenwick (player-level on-ice)**

Player-level on-ice Corsi and Fenwick for a WHL game.

## Usage

``` r
whl_game_corsi(game_id)
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
[`whl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_shifts.md),
[`whl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_summary.md),
[`whl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/whl_leaders.md),
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
 try(whl_game_corsi(game_id = 27225)) 
#> ✖ 2026-06-12 02:23:51.26561: WHL game Corsi for game_id 27225 unavailable! Error in `$<-.data.frame`(`*tmp*`, "corsi_for_per60", value = NA_real_): replacement has 1 row, data has 0
#> data frame with 0 columns and 0 rows
```
