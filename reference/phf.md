# **PHF (Premier Hockey Federation) Endpoint Overview — DEPRECATED**

Wrappers for the now-defunct Premier Hockey Federation (PHF, formerly
NWHL). The PHF ceased operations in mid-2023, so these functions are
kept in the package for historical access to cached data but are
formally **deprecated** in v1.0.0 and emit
[`lifecycle::deprecate_stop()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html).
Tests for these functions check for the `lifecycle_error_deprecated`
class.

## Details

### **Game-level**

|  |  |
|----|----|
| Function | Purpose |
| `phf_game()` | Per-game payload (all, details, raw, summary variants) |
| `phf_game_pbp()` | Per-game play-by-play |
| [`phf_pbp()`](https://fastRhockey.sportsdataverse.org/reference/phf_pbp.md) | Loader entry point (also exports `update_phf_db`) |
| [`phf_player_box()`](https://fastRhockey.sportsdataverse.org/reference/phf_player_box.md) | Per-game player boxscore |
| [`phf_team_box()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_box.md) | Per-game team boxscore |

### **Season-level**

|  |  |
|----|----|
| Function | Purpose |
| [`phf_schedule()`](https://fastRhockey.sportsdataverse.org/reference/phf_schedule.md) | Season schedule |
| [`phf_standings()`](https://fastRhockey.sportsdataverse.org/reference/phf_standings.md) | Standings |
| [`phf_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_roster.md) | Team rosters |
| [`phf_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_stats.md) | Team season stats |
| [`phf_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/phf_player_stats.md) | Player season stats |
| [`phf_leaders()`](https://fastRhockey.sportsdataverse.org/reference/phf_leaders.md) | League leaders |
| [`phf_league_info()`](https://fastRhockey.sportsdataverse.org/reference/phf_league_info.md) | League info |

## Deprecation

All PHF wrappers call
[`lifecycle::deprecate_stop()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html)
and raise errors. Use the still-active
[pwhl](https://fastRhockey.sportsdataverse.org/reference/pwhl.md) family
for current women's pro hockey.
