## Release summary

This is version 1.0.0. Continued development on the 1.0.0 release adds:

* **NHL Edge advanced-metrics integration (33 new functions)** wrapping
  `https://api-web.nhle.com/v1/edge/...` for skater, goalie, and team
  positional / shot-tracking analytics. All wrappers share an internal
  `.nhl_edge_api()` helper and accept an optional `season` argument
  that defaults to the current season via the `/now` form.

* **NHL Records API integration (25 new functions)** -- first-time
  fastRhockey integration with `https://records.nhl.com/site/api/` for
  franchise, player, goalie, draft, award, HOF, official, attendance,
  venue, and combine records. All share an internal
  `.nhl_records_api()` helper that supports `cayenneExp` filtering and
  pagination.

* **NHL Stats REST dedicated wrappers (13 new functions)** that promote
  endpoints previously only reachable via the `nhl_stats_misc()` generic
  dispatcher: `nhl_stats_franchise()`, `nhl_stats_players()`,
  `nhl_stats_glossary()`, `nhl_stats_country()`, `nhl_stats_config()`,
  `nhl_stats_ping()`, `nhl_stats_skater_leaders()`,
  `nhl_stats_goalie_leaders()`, `nhl_stats_skater_milestones()`,
  `nhl_stats_goalie_milestones()`, `nhl_stats_team_listing()`,
  `nhl_stats_game_listing()`, `nhl_stats_content_module()`.

* **NHL api-web miscellaneous endpoints (6 new functions, 3 in-place
  updates)**: `nhl_wsc_pbp()`, `nhl_draft_tracker()`, `nhl_ppt_replay()`,
  `nhl_ppt_replay_goal()`, `nhl_postal_lookup()`, `nhl_smartlinks()`,
  plus `nhl_scoreboard()` (now accepts `date`), `nhl_meta()` (now
  supports playoff-series metadata), and `nhl_draft_year()` (now hits
  the `draft/picks/{year}/all` shortcut when `round` is `NULL` or
  `"all"`).

* **Helper aggregators (6 new functions)** inspired by `nhl-api-py`'s
  `Helpers` and `Stats` modules: `nhl_game_ids_by_season()`,
  `nhl_all_players_by_season()`, `nhl_player_career_stats()`,
  `nhl_team_summary_range()`, `nhl_skater_summary_range()`,
  `nhl_goalie_summary_range()`.

* **OpenAPI 3.0.3 specs and endpoint mapping** for all three NHL API
  backends (`api-web.nhle.com/v1/`, `api.nhle.com/stats/rest/`,
  `records.nhl.com/site/api/`) added to `data-raw/`.

* **Thirteen new NHL season-level loaders**: `load_nhl_skater_box()`,
  `load_nhl_goalie_box()`, `load_nhl_game_rosters()`, `load_nhl_game_info()`,
  `load_nhl_scoring()`, `load_nhl_penalties()`, `load_nhl_three_stars()`,
  `load_nhl_scratches()`, `load_nhl_linescore()`, `load_nhl_shifts()`,
  `load_nhl_officials()`, `load_nhl_shots_by_period()`, `load_nhl_shootout()`
  for pre-compiled datasets hosted on `sportsdataverse-data` GitHub releases.
  The six pre-existing NHL loaders were internally refactored to share a
  single helper (`.nhl_release_loader()`, in `R/nhl_loaders.R`) without
  changing their public signatures. All accept `seasons` (Min: 2011) and
  return `fastRhockey_data`. `nhl_schedule()` also gained an opt-in
  `include_data_flags = FALSE` parameter that joins per-game
  data-availability flags from the data repo onto the live schedule.

* **Eleven new PWHL season-level loaders**: `load_pwhl_skater_box()`,
  `load_pwhl_goalie_box()`, `load_pwhl_team_box()`, `load_pwhl_game_info()`,
  `load_pwhl_scoring_summary()`, `load_pwhl_penalty_summary()`,
  `load_pwhl_three_stars()`, `load_pwhl_officials()`,
  `load_pwhl_shots_by_period()`, `load_pwhl_shootout()`,
  `load_pwhl_game_rosters()`) for pre-compiled datasets hosted on
  `sportsdataverse-data` GitHub releases. The four pre-existing PWHL loaders
  were internally refactored to share a single helper without changing their
  public signatures.

No breaking changes. All previously exported functions retain their
public signatures.

The original 1.0.0 release notes are preserved below.

## Release summary (1.0.0 - initial)

Version 1.0.0 signals API stability for the NHL and PWHL
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
