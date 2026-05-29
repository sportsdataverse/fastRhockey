# **NHL Data Loaders Overview**

Loaders for season-level NHL datasets published as GitHub releases on
`sportsdataverse/sportsdataverse-data`. Each helper is a thin wrapper
around `.nhl_release_loader()` which validates the requested seasons,
builds the per-asset URLs from a `(release_tag, file_prefix)` pair,
downloads in parallel (with optional `progressr` progress + optional
`DBI::DBIConnection` insertion), and tags the result with the
`fastRhockey_data` S3 class. Adding a new dataset is one new row in the
catalog table below.

## Details

### **Loader catalog**

|  |  |  |
|----|----|----|
| Function | Release tag | File prefix |
| [`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md) | `nhl_pbp_full` | `play_by_play` |
| [`load_nhl_pbp_lite()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp_lite.md) | `nhl_pbp_lite` | `play_by_play_lite` |
| [`load_nhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_player_box.md) | `nhl_player_boxscores` | `player_box` |
| [`load_nhl_skater_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_skater_box.md) | `nhl_skater_boxscores` | `skater_box` |
| [`load_nhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_goalie_box.md) | `nhl_goalie_boxscores` | `goalie_box` |
| [`load_nhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_box.md) | `nhl_team_boxscores` | `team_box` |
| [`load_nhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_schedule.md) | `nhl_schedules` | `nhl_schedule` |
| [`load_nhl_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_rosters.md) | `nhl_rosters` | `rosters` |
| [`load_nhl_game_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_game_rosters.md) | `nhl_game_rosters` | `game_rosters` |
| [`load_nhl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_game_info.md) | `nhl_game_info` | `game_info` |
| [`load_nhl_scoring()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_scoring.md) | `nhl_scoring` | `scoring` |
| [`load_nhl_penalties()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_penalties.md) | `nhl_penalties` | `penalties` |
| [`load_nhl_three_stars()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_three_stars.md) | `nhl_three_stars` | `three_stars` |
| [`load_nhl_scratches()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_scratches.md) | `nhl_scratches` | `scratches` |
| [`load_nhl_linescore()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_linescore.md) | `nhl_linescore` | `linescore` |
| [`load_nhl_shifts()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_shifts.md) | `nhl_shifts` | `shifts` |
| [`load_nhl_officials()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_officials.md) | `nhl_officials` | `officials` |
| [`load_nhl_shots_by_period()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_shots_by_period.md) | `nhl_shots_by_period` | `shots_by_period` |
| [`load_nhl_shootout()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_shootout.md) | `nhl_shootout` | `shootout_summary` |

### **DB helpers**

|  |  |
|----|----|
| Function | Purpose |
| [`update_nhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_nhl_db.md) | Idempotent loader → DB writer (delta only) |
| `build_nhl_db()` | Bulk-build a DB from release files |
| `get_missing_nhl_games()` | Show games missing from a DB target |
