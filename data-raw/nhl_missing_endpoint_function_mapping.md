# NHL API — Missing Endpoint → Function Mapping

This document catalogs every NHL API endpoint that is **not yet wrapped by an
exported fastRhockey function** and proposes a target R function name,
parameter signature, and usage pattern for each. It is the working spec for
closing fastRhockey's gap against the public NHL API surface.

## Sources

| Source | Role |
|---|---|
| [dfleis/nhl-api-docs](https://github.com/dfleis/nhl-api-docs) — `data/final/ENDPOINTS.json` | Authoritative endpoint catalog (595 endpoints across 3 base URLs) |
| [RentoSaijo/nhlscraper wiki — pages 1, 2, 3](https://github.com/RentoSaijo/nhlscraper/wiki) | Per-endpoint descriptions for api-web, stats REST, and records APIs |
| [coreyjs/nhl-api-py](https://github.com/coreyjs/nhl-api-py) + [endpoints wiki](https://github.com/coreyjs/nhl-api-py/wiki/NHL-API-Endpoints-2025-2026) | Peer Python client — confirms which endpoints have community demand |

The full endpoint inventory is also available as OpenAPI 3.0 specs in this
directory:

- [`nhl_api_web_openapi.json`](nhl_api_web_openapi.json) / [`.yaml`](nhl_api_web_openapi.yaml) — 132 endpoints
- [`nhl_stats_rest_openapi.json`](nhl_stats_rest_openapi.json) / [`.yaml`](nhl_stats_rest_openapi.yaml) — 21 endpoints
- [`nhl_records_openapi.json`](nhl_records_openapi.json) / [`.yaml`](nhl_records_openapi.yaml) — 442 endpoints

## Coverage summary (verified against `R/` source via grep)

| Backend | Documented | Wrapped | Missing |
|---|---:|---:|---:|
| `api-web.nhle.com/v1/` | 132 | 51 | **81** |
| `api.nhle.com/stats/rest/{lang}/` | 21 | 6 dedicated + generic dispatcher | **15** |
| `records.nhl.com/site/api/` | 442 | 0 | **442** |
| **Total** | **595** | **57** | **538** |

## Conventions used in this document

- **Function name** — proposed snake_case R name following existing
  fastRhockey conventions (`league_entity_action()`, e.g.
  `nhl_edge_skater_detail()`).
- **Method** — always GET for these APIs.
- **R parameters** — what the wrapper function should accept. Optional
  parameters carry a default; required parameters do not.
- **Returns** — what the parsed `fastRhockey_data` payload should look like
  (data frame, list, or list of frames).
- **Notes** — implementation hints, related endpoints, or peer-library
  reference.

All proposed wrappers should:

1. Tag responses with `make_fastRhockey_data()` (S3 class
   `fastRhockey_data`) per [CLAUDE.md](../CLAUDE.md).
2. Use `httr::RETRY("GET", ...)` and `tryCatch` returning `NULL` on failure.
3. Run responses through `janitor::clean_names()`.
4. Live in one file per exported function under [R/](../R/), filename
   matching function name.

---

## 1. NHL Web API — `api-web.nhle.com/v1/`

### 1a. NHL Edge Analytics — 66 endpoints, **0 currently wrapped**

The NHL Edge family is the largest single integration gap. Every endpoint pairs
a `/{season}/{gameType}` form with a `/now` form, so the proposed wrappers
collapse each pair behind a `season` argument that defaults to `NULL` (current
season). All Edge endpoints return positional / shot-tracking advanced metrics
introduced in 2023.

**Architecture proposal:** add an internal helper `.nhl_edge_api(path,
params)` analogous to [`pwhl_helpers.R`](../R/pwhl_helpers.R)'s `.pwhl_api()`
to centralize the URL build, retry, and JSON parse.

#### Skater Edge — 14 wrapper functions / 28 endpoints

| Proposed function | Endpoint(s) | Required params | Optional params | Returns | Notes / usage |
|---|---|---|---|---|---|
| `nhl_edge_skater_detail()` | `edge/skater-detail/{playerId}/{season}/{gameType}`, `edge/skater-detail/{playerId}/now` | `player_id` | `season=NULL`, `game_type=2` | data frame of advanced metrics | "give me Auston Matthews' Edge profile this year" |
| `nhl_edge_skater_landing()` | `edge/skater-landing/{season}/{gameType}`, `edge/skater-landing/now` | — | `season=NULL`, `game_type=2` | landing page payload (list) | League-wide skater landing |
| `nhl_edge_skater_comparison()` | `edge/skater-comparison/{playerId}/{season}/{gameType}`, `edge/skater-comparison/{playerId}/now` | `player_id` | `season=NULL`, `game_type=2` | data frame: focus player vs cohort | Pair-wise / cohort comparison view |
| `nhl_edge_skater_shot_location_detail()` | `edge/skater-shot-location-detail/{playerId}/...` (+ `/now`) | `player_id` | `season=NULL`, `game_type=2` | data frame of shot locations | Heatmap data per skater |
| `nhl_edge_skater_shot_location_top_10()` | `edge/skater-shot-location-top-10/{position}/{category}/{sortBy}/...` (+ `/now`) | `position`, `category`, `sort_by` | `season=NULL`, `game_type=2` | top-10 leaderboard | League-wide leaderboard |
| `nhl_edge_skater_shot_speed_detail()` | `edge/skater-shot-speed-detail/{playerId}/...` (+ `/now`) | `player_id` | `season=NULL`, `game_type=2` | data frame of shot speeds | Shot velocity tracking |
| `nhl_edge_skater_shot_speed_top_10()` | `edge/skater-shot-speed-top-10/{positions}/{sortBy}/...` (+ `/now`) | `positions`, `sort_by` | `season=NULL`, `game_type=2` | top-10 leaderboard | Hardest shot leaders |
| `nhl_edge_skater_skating_speed_detail()` | `edge/skater-skating-speed-detail/{playerId}/...` (+ `/now`) | `player_id` | `season=NULL`, `game_type=2` | data frame | Skating velocity bursts |
| `nhl_edge_skater_speed_top_10()` | `edge/skater-speed-top-10/{positions}/{sortBy}/...` (+ `/now`) | `positions`, `sort_by` | `season=NULL`, `game_type=2` | top-10 leaderboard | Fastest skaters |
| `nhl_edge_skater_skating_distance_detail()` | `edge/skater-skating-distance-detail/{playerId}/...` (+ `/now`) | `player_id` | `season=NULL`, `game_type=2` | data frame | Distance covered |
| `nhl_edge_skater_distance_top_10()` | `edge/skater-distance-top-10/{positions}/{strength}/{sortBy}/...` (+ `/now`) | `positions`, `strength`, `sort_by` | `season=NULL`, `game_type=2` | top-10 leaderboard | Iron-man / mileage leaders |
| `nhl_edge_skater_zone_time()` | `edge/skater-zone-time/{playerId}/...` (+ `/now`) | `player_id` | `season=NULL`, `game_type=2` | data frame | Offensive/defensive zone time |
| `nhl_edge_skater_zone_time_top_10()` | `edge/skater-zone-time-top-10/{positions}/{strength}/{sortBy}/...` (+ `/now`) | `positions`, `strength`, `sort_by` | `season=NULL`, `game_type=2` | top-10 leaderboard | Zone-time leaders |
| `nhl_cat_edge_skater_detail()` | `cat/edge/skater-detail/{playerId}/...` (+ `/now`) | `player_id` | `season=NULL`, `game_type=2` | data frame | Categorical (CAT) Edge variant |

#### Goalie Edge — 9 wrapper functions / 18 endpoints

| Proposed function | Endpoint(s) | Required params | Optional params | Returns | Notes / usage |
|---|---|---|---|---|---|
| `nhl_edge_goalie_detail()` | `edge/goalie-detail/{playerId}/...` (+ `/now`) | `player_id` | `season=NULL`, `game_type=2` | data frame | Goalie advanced metrics |
| `nhl_edge_goalie_landing()` | `edge/goalie-landing/...` (+ `/now`) | — | `season=NULL`, `game_type=2` | list payload | League-wide goalie landing |
| `nhl_edge_goalie_comparison()` | `edge/goalie-comparison/{playerId}/...` (+ `/now`) | `player_id` | `season=NULL`, `game_type=2` | data frame | Goalie cohort comparison |
| `nhl_edge_goalie_5v5_detail()` | `edge/goalie-5v5-detail/{playerId}/...` (+ `/now`) | `player_id` | `season=NULL`, `game_type=2` | data frame | Even-strength only |
| `nhl_edge_goalie_5v5_top_10()` | `edge/goalie-5v5-top-10/{sortBy}/...` (+ `/now`) | `sort_by` | `season=NULL`, `game_type=2` | top-10 leaderboard | EV save % leaders |
| `nhl_edge_goalie_save_percentage_detail()` | `edge/goalie-save-percentage-detail/{playerId}/...` (+ `/now`) | `player_id` | `season=NULL`, `game_type=2` | data frame | Save % by zone/danger |
| `nhl_edge_goalie_edge_save_pctg_top_10()` | `edge/goalie-edge-save-pctg-top-10/{sortBy}/...` (+ `/now`) | `sort_by` | `season=NULL`, `game_type=2` | top-10 leaderboard | Edge save % leaders |
| `nhl_edge_goalie_shot_location_detail()` | `edge/goalie-shot-location-detail/{playerId}/...` (+ `/now`) | `player_id` | `season=NULL`, `game_type=2` | data frame | Shots faced by location |
| `nhl_edge_goalie_shot_location_top_10()` | `edge/goalie-shot-location-top-10/{category}/{sortBy}/...` (+ `/now`) | `category`, `sort_by` | `season=NULL`, `game_type=2` | top-10 leaderboard | Best by shot location category |
| `nhl_cat_edge_goalie_detail()` | `cat/edge/goalie-detail/{playerId}/...` (+ `/now`) | `player_id` | `season=NULL`, `game_type=2` | data frame | Categorical Edge variant |

#### Team Edge — 10 wrapper functions / 20 endpoints

| Proposed function | Endpoint(s) | Required params | Optional params | Returns | Notes / usage |
|---|---|---|---|---|---|
| `nhl_edge_team_detail()` | `edge/team-detail/{teamId}/...` (+ `/now`) | `team_id` | `season=NULL`, `game_type=2` | data frame | Team Edge profile |
| `nhl_edge_team_landing()` | `edge/team-landing/...` (+ `/now`) | — | `season=NULL`, `game_type=2` | list payload | League-wide team landing |
| `nhl_edge_team_shot_location_detail()` | `edge/team-shot-location-detail/{teamId}/...` (+ `/now`) | `team_id` | `season=NULL`, `game_type=2` | data frame | Team shot heatmap |
| `nhl_edge_team_shot_location_top_10()` | `edge/team-shot-location-top-10/{position}/{category}/{sortBy}/...` (+ `/now`) | `position`, `category`, `sort_by` | `season=NULL`, `game_type=2` | top-10 leaderboard | League team leaderboard |
| `nhl_edge_team_shot_speed_detail()` | `edge/team-shot-speed-detail/{teamId}/...` (+ `/now`) | `team_id` | `season=NULL`, `game_type=2` | data frame | Team shot velocities |
| `nhl_edge_team_skating_speed_detail()` | `edge/team-skating-speed-detail/{teamId}/...` (+ `/now`) | `team_id` | `season=NULL`, `game_type=2` | data frame | Team skating speeds |
| `nhl_edge_team_skating_speed_top_10()` | `edge/team-skating-speed-top-10/{positions}/{sortBy}/...` (+ `/now`) | `positions`, `sort_by` | `season=NULL`, `game_type=2` | top-10 leaderboard | Team speed leaders |
| `nhl_edge_team_skating_distance_detail()` | `edge/team-skating-distance-detail/{teamId}/...` (+ `/now`) | `team_id` | `season=NULL`, `game_type=2` | data frame | Team distance covered |
| `nhl_edge_team_skating_distance_top_10()` | `edge/team-skating-distance-top-10/{positions}/{strength}/{sortBy}/...` (+ `/now`) | `positions`, `strength`, `sort_by` | `season=NULL`, `game_type=2` | top-10 leaderboard | Team mileage leaders |
| `nhl_edge_team_zone_time_details()` | `edge/team-zone-time-details/{teamId}/...` (+ `/now`) | `team_id` | `season=NULL`, `game_type=2` | data frame | Team zone time |
| `nhl_edge_team_zone_time_top_10()` | `edge/team-zone-time-top-10/{strength}/{sortBy}/...` (+ `/now`) | `strength`, `sort_by` | `season=NULL`, `game_type=2` | top-10 leaderboard | Team zone-time leaders |

### 1b. Other api-web.nhle.com gaps — 15 endpoints

| Proposed function | Endpoint(s) | Required params | Optional params | Returns | Notes / usage |
|---|---|---|---|---|---|
| `nhl_wsc_pbp()` | `wsc/play-by-play/{gameId}` | `game_id` | — | data frame of play events | Narrative-format PBP. Distinct from `gamecenter/{id}/play-by-play` (covered by [nhl_game_pbp()](../R/nhl_game_feed.R#L61)) and from `wsc/game-story` (covered by [nhl_game_story()](../R/nhl_game_story.R#L17)) |
| Update `nhl_scoreboard()` | `scoreboard/{date}` | — | `date=NULL` | scoreboard payload | One-line extension to [nhl_scoreboard.R](../R/nhl_scoreboard.R) — currently hardcoded to `/now` |
| Update `nhl_draft_year()` | `draft/picks/{year}/all` | `year` | `round=NULL` | data frame | Single new branch in [nhl_draft_year.R](../R/nhl_draft_year.R): when `round=="all"` or omitted, use `/all` shortcut instead of looping per round |
| `nhl_draft_tracker()` | `draft-tracker/picks/now` | — | — | live draft picks | Live (real-time) draft tracker — distinct from `draft/picks/now` (static) |
| Update `nhl_meta()` | `meta/playoff-series/{year}/{seriesLetter}` | `year`, `series_letter` (mutually exclusive with `game_id`) | — | metadata list | New branch in [nhl_meta.R](../R/nhl_meta.R) — currently only handles `meta` and `meta/game/{id}` |
| `nhl_ppt_replay()` | `ppt-replay/{gameId}/{eventNumber}` | `game_id`, `event_number` | — | replay metadata | Event-level video replay metadata |
| `nhl_ppt_replay_goal()` | `ppt-replay/goal/{gameId}/{eventNumber}` | `game_id`, `event_number` | — | goal replay metadata | Goal-specific variant |
| `nhl_postal_lookup()` | `postal-lookup/{postalCode}` | `postal_code` | — | location/region payload | Geo lookup for broadcast rights |
| `nhl_smartlinks()` | `smartlinks` | — | `handle=NULL` | smart link payload | Used by NHL.com link router |
| ~~`nhl_openapi()`~~ | ~~`model/v1/openapi.json`~~ | — | — | — | **Removed:** all candidate URLs (`/model/v1/openapi.json`, `/openapi.json`, `/v1/openapi.json`, `/stats/rest/openapi.json`) return 404 on the live server. The dfleis catalog lists this path but the endpoint is not currently exposed. |

> **Effectively covered, no work needed:** `schedule/now` ([nhl_schedule.R:27](../R/nhl_schedule.R#L27) defaults to `Sys.Date()`).

---

## 2. NHL Stats REST API — `api.nhle.com/stats/rest/{lang}/`

### Important context

[`nhl_stats_misc.R:153`](../R/nhl_stats_misc.R#L153) interpolates the
`endpoint` argument directly into
`https://api.nhle.com/stats/rest/{lang}/{endpoint}`, so users **can already
reach** `franchise`, `players`, `glossary`, `country`, etc. via
`nhl_stats_misc(endpoint = "franchise")`. The blocker is **discoverability**:
the roxygen docstring at
[`nhl_stats_misc.R:127-128`](../R/nhl_stats_misc.R#L127-L128) only enumerates
`glossary`, `gameType`, `shiftcharts`, `componentSeason`.

**Action item independent of any new wrappers:** update the roxygen for
`nhl_stats_misc()` to enumerate all 14 valid endpoint values listed below.

### Proposed dedicated wrappers

| Proposed function | Endpoint | Required params | Optional params | Returns | Notes / usage |
|---|---|---|---|---|---|
| `nhl_stats_franchise()` | `{lang}/franchise` | — | `lang="en"`, `limit=100`, `start=0` | data frame of franchises | High-value: parallels `nhl-api-py.Teams.franchises()` |
| `nhl_stats_players()` | `{lang}/players` | — | `lang="en"`, `limit=100`, `start=0`, `cayenne_exp=NULL` | data frame of players | Full player roster dump |
| `nhl_stats_glossary()` | `{lang}/glossary` | — | `lang="en"` | data frame of stat definitions | "What does PIM mean?" — used by `nhl-api-py.Misc.glossary()` |
| `nhl_stats_country()` | `{lang}/country` | — | `lang="en"` | data frame of countries | Country lookup for player nationality |
| `nhl_stats_config()` | `{lang}/config` | — | `lang="en"` | configuration list | Used by `nhl-api-py.Misc.config()` |
| `nhl_stats_skater_leaders()` | `{lang}/leaders/skaters/{attribute}` | `attribute` | `lang="en"`, `cayenne_exp=NULL` | data frame leaderboard | Stats-API skater leaderboards (distinct from api-web `skater-stats-leaders`) |
| `nhl_stats_goalie_leaders()` | `{lang}/leaders/goalies/{attribute}` | `attribute` | `lang="en"`, `cayenne_exp=NULL` | data frame leaderboard | Stats-API goalie leaderboards |
| `nhl_stats_skater_milestones()` | `{lang}/milestones/skaters` | — | `lang="en"`, `cayenne_exp=NULL` | data frame | Skater milestone achievements |
| `nhl_stats_goalie_milestones()` | `{lang}/milestones/goalies` | — | `lang="en"`, `cayenne_exp=NULL` | data frame | Goalie milestone achievements |
| `nhl_stats_team_listing()` | `{lang}/team`, `{lang}/team/id/{id}` | — | `team_id=NULL`, `lang="en"` | data frame of teams or single team | Top-level team listing — distinct from `team/{report}` ([nhl_stats_teams.R](../R/nhl_stats_teams.R)) |
| `nhl_stats_game_listing()` | `{lang}/game` | — | `lang="en"`, `cayenne_exp=NULL`, `limit=100`, `start=0` | data frame of games | Stats-API game listing |
| `nhl_stats_content_module()` | `{lang}/content/module/{templateKey}` | `template_key` | `lang="en"` | content module list | NHL.com CMS content modules |
| `nhl_stats_ping()` | `ping` | — | — | health-check payload | Health check |

---

## 3. NHL Records API — `records.nhl.com/site/api/`

**0 of 442 endpoints are wrapped.** Architecturally identical to the Stats
REST API: every endpoint is `https://records.nhl.com/site/api/{resource}`
with optional `cayenneExp` query filters and `limit` / `start` pagination.

**Architecture proposal:** add an internal helper
`.nhl_records_api(resource, cayenne_exp = NULL, sort = NULL, limit = NULL,
start = NULL, lang = "en")` mirroring [`pwhl_helpers.R`](../R/pwhl_helpers.R)'s
`.pwhl_api()`. Then ship dedicated wrappers for the high-value subset rather
than chasing all 442. The remaining ~370 situational-streak endpoints can be
reached via the helper directly.

### Tier 1 — must-have (~25 wrappers)

These give analysts the most leverage and parallel widely-used queries.

| Proposed function | Resource | Required params | Optional params | Returns | Notes / usage |
|---|---|---|---|---|---|
| `nhl_records_franchise()` | `franchise` | — | `franchise_id=NULL` | data frame | Franchise listing |
| `nhl_records_franchise_detail()` | `franchise-detail` | — | `franchise_id=NULL` | data frame | Detailed franchise metadata |
| `nhl_records_franchise_totals()` | `franchise-totals` | — | `franchise_id=NULL` | data frame | All-time franchise totals |
| `nhl_records_franchise_team_totals()` | `franchise-team-totals` | — | `franchise_id=NULL` | data frame | Per-team-name (relocated franchises) totals |
| `nhl_records_franchise_season_results()` | `franchise-season-results` | — | `franchise_id=NULL` | data frame | Franchise season-by-season results |
| `nhl_records_franchise_playoff_appearances()` | `franchise-playoff-appearances` | — | `franchise_id=NULL` | data frame | Playoff appearance counts |
| `nhl_records_player()` | `player` (+ `player/{id}`) | — | `player_id=NULL` | data frame | Player listing or single player |
| `nhl_records_player_byteam()` | `player/byTeam/{teamId}` | `team_id` | — | data frame | All players who suited up for a team |
| `nhl_records_player_stats()` | `player-stats` | — | `cayenne_exp=NULL` | data frame | Career player stats |
| `nhl_records_skater_real_time_stats_season()` | `skater-real-time-stats-season` | — | `cayenne_exp=NULL` | data frame | Hits/giveaways/takeaways/blocks/faceoffs/TOI by season |
| `nhl_records_skater_real_time_stats_career()` | `skater-real-time-stats-career` | — | `cayenne_exp=NULL` | data frame | Same, career totals |
| `nhl_records_goalie_career_stats()` | `goalie-career-stats` | — | `cayenne_exp=NULL` | data frame | Career goalie stats |
| `nhl_records_goalie_season_stats()` | `goalie-season-stats` | — | `cayenne_exp=NULL` | data frame | Season-by-season goalie stats |
| `nhl_records_goalie_shutout_streak()` | `goalie-shutout-streak` | — | `cayenne_exp=NULL` | data frame | Goalie shutout streaks |
| `nhl_records_draft()` | `draft` | — | `cayenne_exp=NULL` | data frame | Draft listing |
| `nhl_records_draft_lottery_odds()` | `draft-lottery-odds` | — | — | data frame | Lottery odds |
| `nhl_records_draft_lottery_picks()` | `draft-lottery-picks` | — | — | data frame | Lottery results |
| `nhl_records_draft_prospect()` | `draft-prospect` (+ `/{id}`) | — | `prospect_id=NULL` | data frame | Prospect listing |
| `nhl_records_trophy()` | `trophy` | — | — | data frame | Trophies |
| `nhl_records_award_details()` | `award-details` (+ `/{franchiseId}`) | — | `franchise_id=NULL` | data frame | Award winners |
| `nhl_records_hof_players()` | `hof/players` (+ `/{officeId}`) | — | `office_id=NULL` | data frame | Hall of Fame inductees |
| `nhl_records_officials()` | `officials` (+ `/{type}`) | — | `type=NULL` | data frame | NHL on-ice officials |
| `nhl_records_attendance()` | `attendance` | — | — | data frame | Season attendance |
| `nhl_records_venue()` | `venue` | — | — | data frame | Arena listing |
| `nhl_records_combine()` | `combine` | — | — | data frame | Draft combine measurements |

### Tier 2 — high-value (~15 wrappers)

| Proposed function | Resource | Returns / use |
|---|---|---|
| `nhl_records_milestone_tracker()` | `milestone-tracker` | Live milestone watchlist |
| `nhl_records_milestone_500_goal_career()` | `milestone-500-goal-career` | 500-goal scorers |
| `nhl_records_milestone_1000_point_career()` | `milestone-1000-point-career` | 1000-point club |
| `nhl_records_milestone_50_goal_season()` | `milestone-50-goal-season` | 50-goal seasons |
| `nhl_records_milestone_100_point_season()` | `milestone-100-point-season` | 100-point seasons |
| `nhl_records_playoff_series()` | `playoff-series` (+ `/byFinal`, `/bySeason`, `/bySeries`) | Playoff series outcomes |
| `nhl_records_playoff_team_series_stats()` | `playoff-team-series-stats` | Per-team playoff series stats |
| `nhl_records_playoff_skater_series_stats()` | `playoff-skater-series-stats` | Per-skater playoff series stats |
| `nhl_records_playoff_goalie_series_stats()` | `playoff-goalie-series-stats` | Per-goalie playoff series stats |
| `nhl_records_playoff_captains()` | `playoff-captains` | Playoff captains by year |
| `nhl_records_general_manager()` | `general-manager` | GM listing |
| `nhl_records_general_manager_career_records()` | `general-manager-career-records` | GM career records |
| `nhl_records_coach()` | `coach` (+ `/{id}`) | Head coach listing |
| `nhl_records_coach_career_records()` | `coach-career-records` | Coach career records |
| `nhl_records_team_by_game_stats()` | `team-by-game-stats` | Team game-by-game stats |

### Tier 3 — niche, reach via the generic helper

The remaining ~400 endpoints (situational streaks, fastest-goal records,
international tournaments, all-star, expansion drafts, etc.) should be
reachable via the generic helper:

```r
.nhl_records_api(
  resource    = "skater-multi-goal-games-season",
  cayenne_exp = "playerId=8478402",
  limit       = 100
)
```

A complete enumeration with full path templates is in
[`nhl_records_openapi.json`](nhl_records_openapi.json) /
[`nhl_records_openapi.yaml`](nhl_records_openapi.yaml).

---

## 4. Helper / aggregator functions inspired by `nhl-api-py`

These are convenience aggregators that combine multiple endpoints into a
single tidy frame. None correspond to a single API endpoint.

| Proposed function | Mirrors | What it does |
|---|---|---|
| `nhl_game_ids_by_season(season, game_types = c(2L))` | `Helpers.game_ids_by_season` | Iterates every team's `club-schedule-season/{team}/{season}` and returns the union of game IDs |
| `nhl_all_players_by_season(season)` | `Helpers.all_players` | Iterates every team's `roster/{team}/{season}` and flattens forwards + defensemen + goalies |
| `nhl_player_career_stats(player_id)` | `Stats.player_career_stats` | Combines `player/{id}/landing` with the season-by-season game-log totals into a single career frame |
| `nhl_team_summary_range(start_season, end_season)` | `Stats.team_summary` | Calls `team/summary` for a season range and concatenates |
| `nhl_skater_summary_range(start_season, end_season)` | `Stats.skater_stats_summary` | Same for skater summary |
| `nhl_goalie_summary_range(start_season, end_season)` | `Stats.goalie_stats_summary` | Same for goalie summary |

---

## Implementation priority (recommended)

1. **NHL Edge family** (33 wrappers, 66 endpoints) — biggest functional gap;
   peer Python lib has it.
2. **Document existing `nhl_stats_misc()` flexibility** — one-line roxygen
   fix unblocking 13 endpoints.
3. **`nhl_wsc_pbp()`** — one new file, one unique data type.
4. **Update `nhl_scoreboard()`** to accept `date`.
5. **Update `nhl_meta()`** for `meta/playoff-series/...`.
6. **Update `nhl_draft_year()`** for `round = "all"`.
7. **`nhl_ppt_replay()` / `nhl_ppt_replay_goal()`**.
8. **Records API integration** — `.nhl_records_api()` helper + Tier 1
   wrappers (~25 functions). Tier 2 in a follow-up.
9. **Helper aggregators** — `nhl_game_ids_by_season()`,
   `nhl_all_players_by_season()`, `nhl_player_career_stats()`.

---

## Method note

This document is reproducible. The endpoint catalog was downloaded with:

```bash
curl -sL https://raw.githubusercontent.com/dfleis/nhl-api-docs/master/data/final/ENDPOINTS.json \
  -o data-raw/_endpoints_catalog.json
```

The OpenAPI specs in this directory were generated from that catalog with
[`_gen_openapi.py`](_gen_openapi.py). Re-run that script to refresh the
specs whenever the upstream catalog updates.
