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
