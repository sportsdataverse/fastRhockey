# **fastRhockey 1.0.0 (continued development)**

### **PWHL parity: 3 new NHL loaders + datasets**

Three new season-level NHL loaders that bring NHL coverage in line with
PWHL. Each is backed by a new release tag on
[`sportsdataverse/sportsdataverse-data`](https://github.com/sportsdataverse/sportsdataverse-data)
and powered by new extractors in `fastRhockey-nhl-raw/R/scrape_nhl_raw.R`
and `fastRhockey-nhl-data/R/nhl_data_creation.R`.

| function                       | release tag             | rows per game                |
|--------------------------------|-------------------------|------------------------------|
| `load_nhl_officials()`         | `nhl_officials`         | one per official (refs+lins) |
| `load_nhl_shots_by_period()`   | `nhl_shots_by_period`   | one per team per period      |
| `load_nhl_shootout()`          | `nhl_shootout`          | one per shootout attempt     |

### **`nhl_schedule()` data-availability flags**

`nhl_schedule()` gained an `include_data_flags = FALSE` parameter. When
`TRUE`, the live schedule is left-joined against the cached
`nhl_games_in_data_repo` index from the data repo and gains 16 logical
columns (`PBP`, `team_box`, `player_box`, `skater_box`, `goalie_box`,
`game_info`, `game_rosters`, `scoring`, `penalties`, `scratches`,
`linescore`, `three_stars`, `shifts`, `officials`, `shots_by_period`,
`shootout`) telling you which pre-compiled datasets cover each game.
The schedule files in `fastRhockey-nhl-raw/nhl/schedules/` and the
master schedule on the data repo carry these same flags.

### **Richer per-game JSON in `fastRhockey-nhl-raw`**

`scrape_nhl_raw.R` now writes four additional top-level keys into both
`nhl/json/raw/{game_id}.json` and `nhl/json/final/{game_id}.json`:

* `officials` — list of `{role, name}` (referees first, linesmen second)
* `shots_by_period` — long-form per-team-per-period shot totals
* `shootout` — per-attempt summary (NULL if game didn't reach SO)
* `plays_by_period` — OLD-format index of `play_indices` per period
* `on_ice` / `on_ice_plus` / `penalty_box` (final-state reconstruction,
  derived from the processed PBP) — populated only in the `final/`
  variant since they need fastRhockey-enriched on-ice columns

### **Refactor: NHL loaders consolidated into `R/nhl_loaders.R`**

All NHL season-level loaders and the shared `.nhl_release_loader()`
helper now live in `R/nhl_loaders.R`, mirroring the PWHL convention
(`R/pwhl_loaders.R`). The empty `R/nhl_pbp.R` file was removed since
it never contained an actual `nhl_pbp()` scraper — only loaders.
Public signatures and return types are unchanged.

### **New NHL Edge Analytics (33 functions)**

Wraps the NHL Edge advanced-metrics endpoints under
`https://api-web.nhle.com/v1/edge/...`. Every wrapper accepts an optional
`season` argument (4-digit end year, e.g., `2025`) — when omitted, the
`/now` form is used to fetch the current season. All wrappers share an
internal `.nhl_edge_api()` helper in `R/helpers_nhl_edge.R`.

| family  | functions |
|---------|-----------|
| Skater  | `nhl_edge_skater_detail()`, `nhl_edge_skater_landing()`, `nhl_edge_skater_comparison()`, `nhl_edge_skater_shot_location_detail()`, `nhl_edge_skater_shot_location_top_10()`, `nhl_edge_skater_shot_speed_detail()`, `nhl_edge_skater_shot_speed_top_10()`, `nhl_edge_skater_skating_speed_detail()`, `nhl_edge_skater_speed_top_10()`, `nhl_edge_skater_skating_distance_detail()`, `nhl_edge_skater_distance_top_10()`, `nhl_edge_skater_zone_time()`, `nhl_edge_skater_zone_time_top_10()`, `nhl_cat_edge_skater_detail()` |
| Goalie  | `nhl_edge_goalie_detail()`, `nhl_edge_goalie_landing()`, `nhl_edge_goalie_comparison()`, `nhl_edge_goalie_5v5_detail()`, `nhl_edge_goalie_5v5_top_10()`, `nhl_edge_goalie_save_percentage_detail()`, `nhl_edge_goalie_edge_save_pctg_top_10()`, `nhl_edge_goalie_shot_location_detail()`, `nhl_edge_goalie_shot_location_top_10()`, `nhl_cat_edge_goalie_detail()` |
| Team    | `nhl_edge_team_detail()`, `nhl_edge_team_landing()`, `nhl_edge_team_shot_location_detail()`, `nhl_edge_team_shot_location_top_10()`, `nhl_edge_team_shot_speed_detail()`, `nhl_edge_team_skating_speed_detail()`, `nhl_edge_team_skating_speed_top_10()`, `nhl_edge_team_skating_distance_detail()`, `nhl_edge_team_skating_distance_top_10()`, `nhl_edge_team_zone_time_details()`, `nhl_edge_team_zone_time_top_10()` |

### **New NHL Records API integration (25 functions)**

First-time integration with `https://records.nhl.com/site/api/`. All
wrappers share an internal `.nhl_records_api()` helper in
`R/helpers_nhl_records.R` that supports `cayenneExp` filters,
`limit`/`start` pagination, and the records-API response shape
(`{data, total}`).

* Franchise: `nhl_records_franchise()`, `nhl_records_franchise_detail()`,
  `nhl_records_franchise_totals()`, `nhl_records_franchise_team_totals()`,
  `nhl_records_franchise_season_results()`,
  `nhl_records_franchise_playoff_appearances()`
* Player: `nhl_records_player()`, `nhl_records_player_byteam()`,
  `nhl_records_player_stats()`,
  `nhl_records_skater_real_time_stats_season()`,
  `nhl_records_skater_real_time_stats_career()`
* Goalie: `nhl_records_goalie_career_stats()`,
  `nhl_records_goalie_season_stats()`, `nhl_records_goalie_shutout_streak()`
* Draft: `nhl_records_draft()`, `nhl_records_draft_lottery_odds()`,
  `nhl_records_draft_lottery_picks()`, `nhl_records_draft_prospect()`
* Awards / HOF: `nhl_records_trophy()`, `nhl_records_award_details()`,
  `nhl_records_hof_players()`
* League: `nhl_records_officials()`, `nhl_records_attendance()`,
  `nhl_records_venue()`, `nhl_records_combine()`

### **New NHL Stats REST wrappers (13 functions)**

Promotes endpoints that were previously only reachable via
`nhl_stats_misc()`'s generic dispatcher into discoverable, dedicated
wrappers. Documentation for `nhl_stats_misc()` was also updated to
enumerate every valid `endpoint` value.

* `nhl_stats_franchise()`, `nhl_stats_players()`, `nhl_stats_glossary()`,
  `nhl_stats_country()`, `nhl_stats_config()`, `nhl_stats_ping()`
* `nhl_stats_skater_leaders()`, `nhl_stats_goalie_leaders()`,
  `nhl_stats_skater_milestones()`, `nhl_stats_goalie_milestones()`
* `nhl_stats_team_listing()`, `nhl_stats_game_listing()`,
  `nhl_stats_content_module()`

### **New NHL api-web miscellaneous endpoints (6 functions + 3 in-place updates)**

* `nhl_wsc_pbp(game_id)` -- narrative-format play-by-play from
  `wsc/play-by-play/{gameId}` (distinct from `nhl_game_pbp()` which uses
  `gamecenter/{id}/play-by-play`).
* `nhl_draft_tracker()` -- live draft tracker
  (`draft-tracker/picks/now`), distinct from `nhl_draft()` which hits the
  static `draft/picks/now` endpoint.
* `nhl_ppt_replay(game_id, event_number)` -- event-level replay metadata.
* `nhl_ppt_replay_goal(game_id, event_number)` -- goal-specific replay
  metadata.
* `nhl_postal_lookup(postal_code)` -- broadcast region lookup by postal
  code.
* `nhl_smartlinks(handle = NULL)` -- NHL.com smart-link router.
* `nhl_scoreboard(date = NULL)` -- now accepts an optional `date`
  argument so callers can fetch historical scoreboards (was hardcoded to
  `/now`).
* `nhl_meta(game_id = NULL, year = NULL, series_letter = NULL)` --
  added a third branch for `meta/playoff-series/{year}/{seriesLetter}`.
* `nhl_draft_year(year, round = NULL)` -- when `round` is `NULL` or
  `"all"`, the function now hits the `/draft/picks/{year}/all` shortcut
  in a single request instead of looping per round.

### **New helper aggregators inspired by `nhl-api-py` (6 functions)**

Convenience wrappers that orchestrate multiple endpoint calls into one
tidy data frame:

* `nhl_game_ids_by_season(season, game_types, team_abbr, sleep_rate)` --
  iterates every team's season schedule and returns the deduplicated set
  of game IDs.
* `nhl_all_players_by_season(season, sleep_rate)` -- iterates every
  team's roster and flattens forwards/defensemen/goalies.
* `nhl_player_career_stats(player_id)` -- combines `player/{id}/landing`
  with the season-by-season `seasonTotals` payload into a career frame.
* `nhl_team_summary_range(start_season, end_season)` -- multi-season
  team summary loop over `nhl_stats_teams(report_type = "summary")`.
* `nhl_skater_summary_range(start_season, end_season)` -- same for
  skater summary.
* `nhl_goalie_summary_range(start_season, end_season)` -- same for
  goalie summary.

### **Endpoint mapping + OpenAPI specs**

* `data-raw/nhl_missing_endpoint_function_mapping.md` -- table of every
  documented NHL API endpoint and its proposed/implemented
  fastRhockey wrapper, sourced from
  [dfleis/nhl-api-docs](https://github.com/dfleis/nhl-api-docs),
  [RentoSaijo/nhlscraper](https://github.com/RentoSaijo/nhlscraper),
  and [coreyjs/nhl-api-py](https://github.com/coreyjs/nhl-api-py).
* `data-raw/nhl_api_web_openapi.{json,yaml}` -- OpenAPI 3.0.3 spec for
  `api-web.nhle.com/v1/` (132 endpoints).
* `data-raw/nhl_stats_rest_openapi.{json,yaml}` -- OpenAPI 3.0.3 spec
  for `api.nhle.com/stats/rest/{lang}/` (21 endpoints).
* `data-raw/nhl_records_openapi.{json,yaml}` -- OpenAPI 3.0.3 spec for
  `records.nhl.com/site/api/` (442 endpoints).
* `data-raw/_gen_openapi.py` -- regenerator script.

### **Bug Fixes / API quirks documented**

* `nhl_stats_players()` now detects `data: []` empty-list responses
  (the API returns this when no `cayenne_exp` filter is supplied) and
  returns `NULL` with a friendly hint instead of crashing
  `clean_names()`.
* `nhl_stats_skater_leaders()` and `nhl_stats_goalie_leaders()` no
  longer pass `start`/`limit` query parameters, which the
  `leaders/{skaters,goalies}/{attribute}` endpoint rejects with a 500
  Cayenne SQL error. Roxygen now documents the valid `attribute`
  values: skaters accept `assists`/`goals`/`points`; goalies accept
  `savePctg`/`gaa`/`shutouts`.
* `nhl_records_franchise(franchise_id = ...)` now translates the
  `franchise_id` argument into a `cayenneExp=id={id}` filter. The
  records API does not support a path-suffix `franchise/{id}` form
  (returns 404).
* `nhl_records_award_details()` -- replaced the broken `franchise_id`
  argument with `season_id` (the `award-details` endpoint accepts
  `seasonId` filtering but rejects `franchiseId`).
* `nhl_stats_content_module()` now guards against unnamed CMS
  responses that previously crashed `clean_names()`.

### **New NHL Loaders**

Ten new season-level loaders that pull pre-compiled NHL datasets from new
release tags on
[`sportsdataverse/sportsdataverse-data`](https://github.com/sportsdataverse/sportsdataverse-data).
All accept a `seasons` vector (Min: `2011`) and return a `fastRhockey_data`
data frame, mirroring the existing `load_nhl_*()` API:

| function                   | release tag            | rows per game            |
|----------------------------|------------------------|--------------------------|
| `load_nhl_skater_box()`    | `nhl_skater_boxscores` | one per skater           |
| `load_nhl_goalie_box()`    | `nhl_goalie_boxscores` | one per goalie           |
| `load_nhl_game_rosters()`  | `nhl_game_rosters`     | one per dressed player   |
| `load_nhl_game_info()`     | `nhl_game_info`        | one                      |
| `load_nhl_scoring()`       | `nhl_scoring`          | one per goal             |
| `load_nhl_penalties()`     | `nhl_penalties`        | one per penalty          |
| `load_nhl_three_stars()`   | `nhl_three_stars`      | up to three              |
| `load_nhl_scratches()`     | `nhl_scratches`        | one per scratched player |
| `load_nhl_linescore()`     | `nhl_linescore`        | one                      |
| `load_nhl_shifts()`        | `nhl_shifts`           | one per shift            |

The six pre-existing loaders (`load_nhl_pbp()`, `load_nhl_pbp_lite()`,
`load_nhl_player_box()`, `load_nhl_team_box()`, `load_nhl_schedule()`,
`load_nhl_rosters()`) were refactored to share a single internal worker
(`.nhl_release_loader()`) without changing their public signatures or return
types.

### **New PWHL Loaders**

Eleven new season-level loaders that pull pre-compiled PWHL datasets from
new release tags on
[`sportsdataverse/sportsdataverse-data`](https://github.com/sportsdataverse/sportsdataverse-data).
All accept a `seasons` vector (Min: `2024`) and return a `fastRhockey_data`
data frame, mirroring the existing `load_pwhl_*()` API:

| function                       | release tag                | rows per game        |
|--------------------------------|----------------------------|----------------------|
| `load_pwhl_skater_box()`       | `pwhl_skater_boxscores`    | one per skater       |
| `load_pwhl_goalie_box()`       | `pwhl_goalie_boxscores`    | one per goalie       |
| `load_pwhl_team_box()`         | `pwhl_team_boxscores`      | two (home/away)      |
| `load_pwhl_game_info()`        | `pwhl_game_info`           | one                  |
| `load_pwhl_scoring_summary()`  | `pwhl_scoring_summary`     | one per goal         |
| `load_pwhl_penalty_summary()`  | `pwhl_penalty_summary`     | one per penalty      |
| `load_pwhl_three_stars()`      | `pwhl_three_stars`         | up to three          |
| `load_pwhl_officials()`        | `pwhl_officials`           | one per official     |
| `load_pwhl_shots_by_period()`  | `pwhl_shots_by_period`     | one per period       |
| `load_pwhl_shootout()`         | `pwhl_shootout`            | one per attempt      |
| `load_pwhl_game_rosters()`     | `pwhl_game_rosters`        | one per dressed player|

The four pre-existing loaders (`load_pwhl_pbp()`, `load_pwhl_player_box()`,
`load_pwhl_schedule()`, `load_pwhl_rosters()`) were refactored to share a
single internal worker (`.pwhl_release_loader()`) without changing their
public signatures or return types.

# **fastRhockey 1.0.0**

### **Breaking Changes**

* **Version 1.0.0** signals API stability for NHL and PWHL endpoints.
* All PHF functions are formally deprecated via `lifecycle`. The Premier Hockey
  Federation ceased operations; use PWHL functions instead. Functions will be
  removed in a future release.
* Consolidated new NHL API functions into existing NHL function names rather
  than creating `_v2` variants, since the original API endpoints were
  deprecated by the NHL. This means `nhl_game_feed()`, `nhl_game_boxscore()`,
  `nhl_schedule()`, `nhl_teams()`, `nhl_teams_roster()`, and
  `nhl_player_info()` now use the new `api-web.nhle.com` endpoints directly.

### **New PWHL Functions**

* `pwhl_pbp()` -- PWHL play-by-play data.
* `pwhl_player_box()` -- PWHL player box scores (skaters and goalies).
* `pwhl_game_info()` -- PWHL game information and metadata.
* `pwhl_game_summary()` -- Detailed game summary (scoring, penalties, shots
  by period, three stars).
* `pwhl_standings()` -- PWHL league standings.
* `pwhl_stats()` -- PWHL stat leaders (skaters and goalies).
* `pwhl_leaders()` -- League leaders (top scorers and top goalies).
* `pwhl_season_id()` -- PWHL season ID lookup (now API-driven with fallback).
* `pwhl_player_info()` -- Player biographical and profile information.
* `pwhl_player_game_log()` -- Per-game statistics for a player in a season.
* `pwhl_player_stats()` -- Career and season-by-season statistics for a player.
* `pwhl_player_search()` -- Search for players by name.
* `pwhl_transactions()` -- Player transactions for a season.
* `pwhl_streaks()` -- Player streak data for a season.
* `pwhl_playoff_bracket()` -- Playoff bracket / series data.
* `pwhl_scorebar()` -- Recent and upcoming game scores.
* `most_recent_pwhl_season()` -- Utility to get the current PWHL season year.
* `load_pwhl_pbp()` -- Load pre-scraped PWHL play-by-play data.
* `load_pwhl_player_box()` -- Load pre-scraped PWHL player box scores.
* `load_pwhl_schedule()` -- Load pre-scraped PWHL schedules.
* `load_pwhl_rosters()` -- Load pre-scraped PWHL team rosters.
* `update_pwhl_db()` -- Update or create a PWHL play-by-play database.

### **New NHL Functions**

* `nhl_where_to_watch()` -- streaming/broadcast availability.
* `nhl_partner_game_odds()` -- partner game odds by country.
* NHL API migration to `api-web.nhle.com`: `nhl_game_feed()`,
  `nhl_game_boxscore()`, `nhl_schedule()`, `nhl_teams()`,
  `nhl_teams_roster()`, `nhl_player_info()`, `nhl_player_game_log()`,
  `nhl_stats_goalies()`, `nhl_stats_skaters()`, `nhl_stats_teams()`,
  `nhl_stats_misc()`, `nhl_gamecenter_landing()`,
  `nhl_gamecenter_right_rail()`, `nhl_scoreboard()`, `nhl_scores()`,
  `nhl_seasons()`, `nhl_tv_schedule()`, `nhl_game_story()`,
  `nhl_game_content()`, `nhl_game_shifts()`, `nhl_playoff_bracket()`,
  `nhl_playoff_schedule()`, `nhl_playoff_carousel()`.
* xG model integration via `helper_nhl_calculate_xg()`.

### **Bug Fixes**

* Fixed `nhl_draft_year()` -- NHL API removed the `/v1/draft/picks/{year}`
  endpoint. The function now iterates over rounds 1-7 using
  `/v1/draft/picks/{year}/{round}`.
* Fixed `pwhl_stats()` -- resolved "object 'players' not found" error when
  API calls failed. Fixed team ID resolution for skater stats.
* Fixed `pwhl_schedule()` -- the `season` column was inadvertently dropped
  from the output. It is now included.
* Fixed `refresh_xg_models()` -- resolved "cannot change value of locked
  binding" error by storing xG models in a package environment (`.xg_env`)
  instead of top-level bindings.
* Fixed NAMESPACE: removed `import(tidyverse)` which violated CRAN policy.
  Individual packages (dplyr, tidyr, etc.) are already imported.

### **Deprecations**

* PHF functions: `phf_schedule()`, `phf_standings()`, `phf_pbp()`,
  `phf_player_box()`, `phf_team_box()`, `phf_team_roster()`,
  `phf_team_stats()`, `phf_player_stats()`, `phf_leaders()`,
  `phf_league_info()`, `phf_game_all()`, `phf_game_raw()`,
  `phf_game_details()`, `phf_game_summary()`.
* PHF loaders: `load_phf_pbp()`, `load_phf_team_box()`,
  `load_phf_player_box()`, `load_phf_schedule()`, `load_phf_rosters()`,
  `update_phf_db()`.
* Utility: `most_recent_phf_season()`.

### **Improvements**

* `pwhl_season_id()` now retrieves season data dynamically from the HockeyTech
  API instead of using a hardcoded lookup table. Falls back to hardcoded data
  when the API is unavailable.
* Added internal helpers `.pwhl_api()`, `.pwhl_modulekit_url()`,
  `.pwhl_gc_url()`, and `.pwhl_resolve_season_id()` to reduce JSONP parsing
  boilerplate across PWHL functions.
* Added `lifecycle` package for formal deprecation management.
* Updated `testthat` dependency to `>= 3.0.0`.
* Complete test coverage for all 95 exported functions (482 tests).
* Added environment-controlled test toggles (`RUN_NHL_TESTS`,
  `RUN_PHF_TESTS`, `RUN_PWHL_TESTS`) via `tests/testthat/helper-skip.R`.
* CI workflow environment variables now match `helper-skip.R` names.
* Updated test expectations to match current API responses.
* `nhl_where_to_watch()` returns `NULL` gracefully when the NHL
  `/v1/where-to-watch` endpoint is unavailable.
* Updated `_pkgdown.yml` with reorganized reference sections and deprecated
  function categories.

### **Documentation & Infrastructure**

* Updated `CONTRIBUTING.md` with naming conventions, testing environment
  variables, conventional commits guide, and deprecation process.
* Updated PR and issue templates.
* Updated `CLAUDE.md` and `.github/copilot-instructions.md` to reflect
  v1.0.0 changes and new PWHL endpoints.
* Added `data-raw/pr_devel.md` development scratchpad.
* Added `cran_comments.md` for CRAN submission.
* Updated `_pkgdown.yml` with reorganized PWHL reference sections.
* Added R CMD check CI workflow and pkgdown deployment workflow.

# **fastRhockey 0.7.0**

### **PWHL functions added**

* `pwhl_schedule()` function added.
* `pwhl_team_roster()` function added.
* `pwhl_teams()` function added.

# **fastRhockey 0.6.0**

* Improved resiliency for several PHF functions, updates under the hood.

# **fastRhockey 0.5.0**

* Major improvements to NHL Game PBP Data parsing with shifts in-line via `nhl_game_pbp()` function added to match [`hockeyR`](https://hockeyr.netlify.app).

# **fastRhockey 0.4.2**

* Fixing issues with `phf_league_info()` function for team name inconsistency.

# **fastRhockey 0.4.1**

* Minor logic addition for pbp parsing.
* More under the hood changes to adapt to tidyselect new version guidelines.
* `load_phf_rosters()` function added.
* `load_nhl_rosters()` function added.

# **fastRhockey 0.4.0**

* Updates logic to add Montreal Force to teams lists/parsing.
* Under the hood changes to adapt to tidyselect new version guidelines.

# **fastRhockey 0.3.1**

* Updates documentation per CRAN's request.

# **fastRhockey 0.3.0**

* Add print method for all functions with a time stamp and description of the data.
* Add `phf_team_logos` dataset to package for reference.

# **fastRhockey 0.2.1**

* hotfix to `helper_phf_pbp_data()` penalty code.
* add `try()` to function examples.

# **fastRhockey 0.2.0**

* `espn_nhl_teams()` function added.

# **fastRhockey 0.1.0**

* Prepped for CRAN.

# **fastRhockey 0.0.4**

### Loader functions for PHF

* `load_phf_pbp()` function added.
* `load_phf_team_box()` function added.
* `load_phf_player_box()` function added.
* `load_phf_schedule()` function added.
* `update_phf_db()` function added.

### Player and Team Stats, Leaderboards

* `phf_leaders()` function added.
* `phf_standings()` function added.
* `phf_player_stats()` function added.
* `phf_team_stats()` function added.
* `phf_team_roster()` function added.

# **fastRhockey 0.0.3**

### Function naming convention normalization

* `load_phf_game()` --> `phf_game_all()`
* `load_phf_pbp()` --> `phf_pbp()`
* `load_phf_boxscore()` --> `phf_team_box()`
* `load_phf_raw_data()` --> `phf_game_raw()`

# **fastRhockey 0.0.2**

* Added NHL functions from `powerplay` to `fastRhockey`.

# **fastRhockey 0.0.1**

* Added a `NEWS.md` file to track changes to the package.
