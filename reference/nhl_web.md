# **NHL Web API Endpoint Overview**

Wrappers around the modern NHL Web API at `api-web.nhle.com/v1/...`.
These cover game feeds, schedules, standings, rosters, gamecenter,
draft, scoreboard, scores, meta, location, partner-game, where-to-watch,
smartlinks, postal-lookup, ppt-replay, and WSC PBP.

All responses are clean JSON parsed with
[`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
and wrapped in the `fastRhockey_data` S3 class via
`make_fastRhockey_data()`.

## Details

### **Game-level**

|  |  |  |
|----|----|----|
| Function | Endpoint family | Purpose |
| [`nhl_game_feed()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_feed.md) | `gamecenter/{id}/play-by-play` | Full PBP + rosters + game info |
| [`nhl_game_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_boxscore.md) | `gamecenter/{id}/boxscore` | Player + team boxscore tables |
| [`nhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_shifts.md) | `stats/rest/en/shiftcharts` | Per-shift records (with HTML fallback for empty endpoints) |
| [`nhl_game_story()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_story.md) | `wsc/game-story/{id}` | Narrative-format recap |
| [`nhl_game_content()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_content.md) | `gamecenter/{id}/...` | Media + content |
| [`nhl_gamecenter_landing()`](https://fastRhockey.sportsdataverse.org/reference/nhl_gamecenter_landing.md) | `gamecenter/{id}/landing` | Game-center landing payload |
| [`nhl_wsc_pbp()`](https://fastRhockey.sportsdataverse.org/reference/nhl_wsc_pbp.md) | `wsc/play-by-play/{id}` | WSC narrative-format play-by-play |
| [`nhl_ppt_replay()`](https://fastRhockey.sportsdataverse.org/reference/nhl_ppt_replay.md) | `ppt-replay/...` | Event-level replay metadata |
| [`nhl_ppt_replay_goal()`](https://fastRhockey.sportsdataverse.org/reference/nhl_ppt_replay_goal.md) | `ppt-replay/goal/...` | Goal-specific replay metadata |

### **Schedule / scores**

|  |  |  |
|----|----|----|
| Function | Endpoint family | Purpose |
| [`nhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_schedule.md) | `schedule/{date}` | Daily / season schedule |
| [`nhl_schedule_calendar()`](https://fastRhockey.sportsdataverse.org/reference/nhl_schedule_calendar.md) | `schedule-calendar/{date}` | Calendar view |
| [`nhl_club_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_club_schedule.md) | `club-schedule/{team}/season` | Per-club schedule |
| [`nhl_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/nhl_scoreboard.md) | `scoreboard/{date}/now` | Live + daily scoreboard |
| [`nhl_scores()`](https://fastRhockey.sportsdataverse.org/reference/nhl_scores.md) | `score/{date}` | Scores |
| [`nhl_tv_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_tv_schedule.md) | `network/tv-schedule` | TV broadcast schedule |
| [`nhl_where_to_watch()`](https://fastRhockey.sportsdataverse.org/reference/nhl_where_to_watch.md) | `where-to-watch/{date}` | Region-aware watch info |

### **Standings / seasons**

|  |  |  |
|----|----|----|
| Function | Endpoint family | Purpose |
| [`nhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/nhl_standings.md) | `standings/{date}` | Current / historical standings |
| [`nhl_standings_season()`](https://fastRhockey.sportsdataverse.org/reference/nhl_standings_season.md) | `standings-season` | Season list |
| [`nhl_seasons()`](https://fastRhockey.sportsdataverse.org/reference/nhl_seasons.md) | `seasons` + rankings | Seasons + draft rankings |
| [`nhl_playoff_carousel()`](https://fastRhockey.sportsdataverse.org/reference/nhl_playoff_carousel.md) | `playoff-series/carousel/...` | Playoff carousel |
| [`nhl_playoff_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_playoff_schedule.md) | `playoff-series/{year}/{seriesLetter}` | Playoff schedule |

### **Teams**

|  |  |  |
|----|----|----|
| Function | Endpoint family | Purpose |
| [`nhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams.md) | `teams` | Team info |
| [`nhl_teams_info()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams_info.md) | (via `nhl_teams`) | Team info by abbreviation |
| [`nhl_teams_roster()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams_roster.md) | `roster/{team}/current` | Current roster |
| [`nhl_roster_season()`](https://fastRhockey.sportsdataverse.org/reference/nhl_roster_season.md) | `roster/{team}/{season}` | Roster by season |
| [`nhl_team_prospects()`](https://fastRhockey.sportsdataverse.org/reference/nhl_team_prospects.md) | `prospects/{team}` | Team prospects |
| [`nhl_team_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/nhl_team_scoreboard.md) | `scoreboard/{team}/now` | Team-specific scoreboard |
| [`nhl_team_summary_range()`](https://fastRhockey.sportsdataverse.org/reference/nhl_team_summary_range.md) | (helper) | Multi-season team summary |
| [`nhl_conferences()`](https://fastRhockey.sportsdataverse.org/reference/nhl_conferences.md) | (derived from standings) | Conferences |
| [`nhl_conferences_info()`](https://fastRhockey.sportsdataverse.org/reference/nhl_conferences_info.md) | (derived) | Conference details |
| [`nhl_divisions()`](https://fastRhockey.sportsdataverse.org/reference/nhl_divisions.md) | (derived) | Divisions |
| [`nhl_divisions_info()`](https://fastRhockey.sportsdataverse.org/reference/nhl_divisions_info.md) | (derived) | Division details |

### **Players**

|  |  |  |
|----|----|----|
| Function | Endpoint family | Purpose |
| [`nhl_player_info()`](https://fastRhockey.sportsdataverse.org/reference/nhl_player_info.md) | `player/{id}/landing` | Player biographical info |
| [`nhl_player_game_log()`](https://fastRhockey.sportsdataverse.org/reference/nhl_player_game_log.md) | `player/{id}/game-log` | Player game logs |
| [`nhl_player_spotlight()`](https://fastRhockey.sportsdataverse.org/reference/nhl_player_spotlight.md) | `player-spotlight` | Featured player spotlight |
| [`nhl_skater_summary_range()`](https://fastRhockey.sportsdataverse.org/reference/nhl_skater_summary_range.md) | (helper) | Multi-season skater summary |
| [`nhl_goalie_summary_range()`](https://fastRhockey.sportsdataverse.org/reference/nhl_goalie_summary_range.md) | (helper) | Multi-season goalie summary |
| [`nhl_all_players_by_season()`](https://fastRhockey.sportsdataverse.org/reference/nhl_all_players_by_season.md) | (helper) | All rostered players across teams for a season |

### **Aggregators**

|  |  |  |
|----|----|----|
| Function | Endpoint family | Purpose |
| [`nhl_game_ids_by_season()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_ids_by_season.md) | (helper) | All game IDs across teams for a season |

### **Meta / location / smart-links**

|  |  |  |
|----|----|----|
| Function | Endpoint family | Purpose |
| [`nhl_meta()`](https://fastRhockey.sportsdataverse.org/reference/nhl_meta.md) | `meta` + playoff-series + per-game meta | League / game / series metadata |
| [`nhl_postal_lookup()`](https://fastRhockey.sportsdataverse.org/reference/nhl_postal_lookup.md) | `postal-lookup/{zip}` | Broadcast region by postal code |
| [`nhl_smartlinks()`](https://fastRhockey.sportsdataverse.org/reference/nhl_smartlinks.md) | NHL.com smart-link router | Smart-link resolver |
