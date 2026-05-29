# **PWHL (Professional Women's Hockey League) Endpoint Overview**

Wrappers around the HockeyTech feeds that back the PWHL stats portal,
grouped by what they return. Three feed flavors are used internally
(`statviewfeed`, `modulekit`, `gc`) — see `pwhl_helpers.R` for the
`.pwhl_api()` / `.pwhl_modulekit_url()` / `.pwhl_gc_url()` helpers that
build the JSONP URLs and strip the Angular callbacks.

## Details

### **Game-level**

|  |  |  |
|----|----|----|
| Function | Feed | Purpose |
| [`pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_schedule.md) | statviewfeed | Season schedule |
| [`pwhl_scorebar()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_scorebar.md) | modulekit | Recent + upcoming scorebar |
| [`pwhl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_game_info.md) | statviewfeed | Per-game metadata |
| [`pwhl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_game_summary.md) | gc | Per-game summary (rosters, periods, three stars) |
| [`pwhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_pbp.md) | statviewfeed | Play-by-play |
| [`pwhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_box.md) | statviewfeed | Per-game player boxscore |

### **Team-level**

|  |  |  |
|----|----|----|
| Function | Feed | Purpose |
| [`pwhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_teams.md) | statviewfeed | Team listing |
| [`pwhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_team_roster.md) | statviewfeed | Active roster |
| [`pwhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_standings.md) | statviewfeed | Current standings |

### **Player-level**

|  |  |  |
|----|----|----|
| Function | Feed | Purpose |
| [`pwhl_player_info()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_info.md) | modulekit | Player profile |
| [`pwhl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_stats.md) | modulekit | Career / season stats |
| [`pwhl_player_game_log()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_game_log.md) | modulekit | Game-by-game player log |
| [`pwhl_player_search()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_search.md) | modulekit | Search by name |
| [`pwhl_stats()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_stats.md) | statviewfeed | League-wide stats |
| [`pwhl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_leaders.md) | modulekit | League leaders (top scorers / goalies) |
| [`pwhl_streaks()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_streaks.md) | modulekit | Player streaks |

### **League-level**

|  |  |  |
|----|----|----|
| Function | Feed | Purpose |
| [`pwhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_season_id.md) | modulekit (with hardcoded fallback) | Map (season, game_type) -\> HockeyTech `season_id` |
| [`pwhl_transactions()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_transactions.md) | modulekit | Player transactions |
| [`pwhl_playoff_bracket()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_playoff_bracket.md) | modulekit | Playoff bracket |
| [`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md) | (computed) | Most-recent PWHL season (end-year) |

## Season Convention

PWHL functions use the **end year** of the season (e.g. `2026` for the
2025-26 season), matching
[`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md).
