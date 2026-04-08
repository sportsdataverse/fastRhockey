## Release summary

This is version 1.0.0 that signals API stability for the NHL and PWHL
endpoints. Key changes:

* Formally deprecated all PHF functions via `lifecycle` (league ceased
  operations).
* Consolidated new NHL API functions into existing NHL functions rather than creating `_v2` variants and deprecating the existing ones since the original API endpoints were deprecated.
* Added `lifecycle` package as an Imports dependency for formal deprecation
  management.
* Fixed NAMESPACE.
* Fixed `nhl_draft_year()` to use updated NHL API endpoint format.
* Fixed `pwhl_stats()` scoping bug and team ID resolution.
* Fixed `refresh_xg_models()` locked binding error by using package environment.
* Added complete test coverage for all exported functions (500+ tests passing).

### **New PWHL Functions**

* Player endpoints: `pwhl_player_info()`, `pwhl_player_game_log()`,
  `pwhl_player_stats()`, `pwhl_player_search()`
* League endpoints: `pwhl_leaders()`, `pwhl_transactions()`,
  `pwhl_streaks()`, `pwhl_playoff_bracket()`
* Game endpoints: `pwhl_game_summary()`, `pwhl_scorebar()`
* Data loaders: `load_pwhl_pbp()`, `load_pwhl_player_box()`,
  `load_pwhl_schedule()`, `load_pwhl_rosters()`, `update_pwhl_db()`
* Utilities: `most_recent_pwhl_season()`

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

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
