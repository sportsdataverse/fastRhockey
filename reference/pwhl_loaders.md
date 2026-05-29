# **PWHL Data Loaders Overview**

Loaders for season-level PWHL datasets published as GitHub releases on
`sportsdataverse/sportsdataverse-data`. Each helper is a thin wrapper
around `.pwhl_release_loader()` which validates seasons, builds the
per-asset URLs from a `(release_tag, file_prefix)` pair, downloads in
parallel with optional `progressr` progress + optional
`DBI::DBIConnection` insertion, and tags the result with the
`fastRhockey_data` S3 class. Adding a new dataset is one new row in the
catalog table below.

## Details

### **Loader catalog**

|  |  |  |
|----|----|----|
| Function | Release tag | File prefix |
| [`load_pwhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_pbp.md) | `pwhl_pbp` | `play_by_play` |
| [`load_pwhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_player_box.md) | `pwhl_player_boxscores` | `player_box` |
| [`load_pwhl_skater_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_skater_box.md) | `pwhl_skater_boxscores` | `skater_box` |
| [`load_pwhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_goalie_box.md) | `pwhl_goalie_boxscores` | `goalie_box` |
| [`load_pwhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_team_box.md) | `pwhl_team_boxscores` | `team_box` |
| [`load_pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_schedule.md) | `pwhl_schedules` | `pwhl_schedule` |
| [`load_pwhl_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_rosters.md) | `pwhl_rosters` | `rosters` |
| [`load_pwhl_game_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_game_rosters.md) | `pwhl_game_rosters` | `game_rosters` |
| [`load_pwhl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_game_info.md) | `pwhl_game_info` | `game_info` |
| [`load_pwhl_scoring_summary()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_scoring_summary.md) | `pwhl_scoring_summary` | `scoring_summary` |
| [`load_pwhl_penalty_summary()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_penalty_summary.md) | `pwhl_penalty_summary` | `penalty_summary` |
| [`load_pwhl_three_stars()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_three_stars.md) | `pwhl_three_stars` | `three_stars` |
| [`load_pwhl_officials()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_officials.md) | `pwhl_officials` | `officials` |
| [`load_pwhl_shots_by_period()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_shots_by_period.md) | `pwhl_shots_by_period` | `shots_by_period` |
| [`load_pwhl_shootout()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_shootout.md) | `pwhl_shootout` | `shootout_summary` |

### **DB helper**

|  |  |
|----|----|
| Function | Purpose |
| [`update_pwhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_pwhl_db.md) | Idempotent loader -\> DB writer (delta only) |

## Season Convention

PWHL loaders use the **end year** of the season (e.g. `2026` for the
2025-26 season), matching
[`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md).
