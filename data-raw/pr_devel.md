# PR Development Notes (v1.0.0 - NHL Endpoint Coverage Expansion)

86 new exported functions across NHL Edge analytics, NHL Records API,
NHL Stats REST dedicated wrappers, NHL api-web miscellaneous endpoints,
and helper aggregators. Adds 2 internal helpers, 86 new test files,
3 OpenAPI 3.0.3 specs, and an endpoint→function mapping document.

## Summary

Close fastRhockey's gap against the public NHL API surface documented in
[`dfleis/nhl-api-docs`](https://github.com/dfleis/nhl-api-docs),
[`RentoSaijo/nhlscraper`](https://github.com/RentoSaijo/nhlscraper),
and [`coreyjs/nhl-api-py`](https://github.com/coreyjs/nhl-api-py).
Coverage delta: api-web `51 → 84 paths`, stats-rest `6 → 19 dedicated
wrappers (+ generic dispatcher)`, records-api `0 → 25 wrappers (+ helper
for the remaining 417)`. Result: **0 errors, 0 warnings, 0 notes**.

| backend | documented | wrapped | added |
|---|---:|---:|---:|
| `api-web.nhle.com/v1/` | 132 | 84 | +33 (Edge) +6 (misc) +3 (in-place updates) |
| `api.nhle.com/stats/rest/{lang}/` | 21 | 19 dedicated | +13 |
| `records.nhl.com/site/api/` | 442 | 25 dedicated + helper | +25 |
| convenience aggregators | n/a | 6 | +6 |
| **total new exported functions** | | | **+86** |

## New NHL Edge Analytics (33 functions)

All wrappers share `R/helpers_nhl_edge.R::.nhl_edge_api()` which handles
the `/now` vs `/{season}/{gameType}` URL split. `.nhl_edge_to_df()`
normalizes the assorted Edge response shapes (top-level data.frame,
named list with one data.frame element, or flat list).

| family | functions |
|---|---|
| Skater (14) | `nhl_edge_skater_detail`, `_landing`, `_comparison`, `_shot_location_detail`, `_shot_location_top_10`, `_shot_speed_detail`, `_shot_speed_top_10`, `_skating_speed_detail`, `_speed_top_10`, `_skating_distance_detail`, `_distance_top_10`, `_zone_time`, `_zone_time_top_10`, `nhl_cat_edge_skater_detail` |
| Goalie (10) | `nhl_edge_goalie_detail`, `_landing`, `_comparison`, `_5v5_detail`, `_5v5_top_10`, `_save_percentage_detail`, `_edge_save_pctg_top_10`, `_shot_location_detail`, `_shot_location_top_10`, `nhl_cat_edge_goalie_detail` |
| Team (11) | `nhl_edge_team_detail`, `_landing`, `_shot_location_detail`, `_shot_location_top_10`, `_shot_speed_detail`, `_skating_speed_detail`, `_skating_speed_top_10`, `_skating_distance_detail`, `_skating_distance_top_10`, `_zone_time_details`, `_zone_time_top_10` |

## New NHL Records API integration (25 functions + 1 helper)

First-time integration with `records.nhl.com/site/api/`. All wrappers
share `R/helpers_nhl_records.R::.nhl_records_api()` which supports
`cayenneExp` filtering, `limit`/`start` pagination, and the records-API
response shape (`{data: [...], total: N}`). The remaining ~417
documented records endpoints are reachable via the helper directly.

* Franchise (6): `nhl_records_franchise`, `_franchise_detail`, `_franchise_totals`, `_franchise_team_totals`, `_franchise_season_results`, `_franchise_playoff_appearances`
* Player (5): `nhl_records_player`, `_player_byteam`, `_player_stats`, `_skater_real_time_stats_season`, `_skater_real_time_stats_career`
* Goalie (3): `nhl_records_goalie_career_stats`, `_goalie_season_stats`, `_goalie_shutout_streak`
* Draft (4): `nhl_records_draft`, `_draft_lottery_odds`, `_draft_lottery_picks`, `_draft_prospect`
* Awards/HOF (3): `nhl_records_trophy`, `_award_details`, `_hof_players`
* League (4): `nhl_records_officials`, `_attendance`, `_venue`, `_combine`

## New NHL Stats REST dedicated wrappers (13 functions)

Promotes endpoints previously only reachable via `nhl_stats_misc()`'s
generic dispatcher into discoverable, exported wrappers. Documentation
for `nhl_stats_misc()` was also updated to enumerate every valid
`endpoint` value.

* `nhl_stats_franchise`, `nhl_stats_players`, `nhl_stats_glossary`,
  `nhl_stats_country`, `nhl_stats_config`, `nhl_stats_ping`
* `nhl_stats_skater_leaders`, `nhl_stats_goalie_leaders`,
  `nhl_stats_skater_milestones`, `nhl_stats_goalie_milestones`
* `nhl_stats_team_listing`, `nhl_stats_game_listing`,
  `nhl_stats_content_module`

## New NHL api-web miscellaneous (6 new + 3 in-place updates)

* New: `nhl_wsc_pbp`, `nhl_draft_tracker`, `nhl_ppt_replay`,
  `nhl_ppt_replay_goal`, `nhl_postal_lookup`, `nhl_smartlinks`
* In-place: `nhl_scoreboard(date = NULL)` accepts a `date` arg,
  `nhl_meta(year = NULL, series_letter = NULL)` adds playoff-series
  metadata branch, `nhl_draft_year(round = NULL)` uses the
  `/draft/picks/{year}/all` shortcut when `round` is omitted

## New helper aggregators inspired by `nhl-api-py` (6 functions)

* `nhl_game_ids_by_season(season, game_types, team_abbr, sleep_rate)`
* `nhl_all_players_by_season(season, sleep_rate)`
* `nhl_player_career_stats(player_id)`
* `nhl_team_summary_range(start_season, end_season)`
* `nhl_skater_summary_range(start_season, end_season)`
* `nhl_goalie_summary_range(start_season, end_season)`

## Bug fixes / API quirks documented

* `nhl_stats_players` now handles `data: []` empty-list responses (the
  endpoint returns this when no `cayenne_exp` filter is supplied).
* `nhl_stats_skater_leaders` and `nhl_stats_goalie_leaders` no longer
  pass `start`/`limit` query parameters (the endpoint rejects them with
  a 500 Cayenne SQL error). Roxygen documents the valid `attribute`
  values: skaters accept `assists`/`goals`/`points`; goalies accept
  `savePctg`/`gaa`/`shutouts`.
* `nhl_records_franchise(franchise_id = ...)` translates the argument
  into a `cayenneExp=id={id}` filter (the path-suffix `franchise/{id}`
  form returns 404).
* `nhl_records_award_details` -- replaced the broken `franchise_id`
  argument with `season_id` (the endpoint accepts `seasonId` filtering
  but rejects `franchiseId`).
* `nhl_stats_content_module` now guards against unnamed CMS responses.
* Removed `nhl_openapi_spec` -- the documented `model/v1/openapi.json`
  endpoint returns 404 across every candidate URL on the live server.
* The 12 NHL Edge top-10 endpoints currently return 404 across every
  observed parameter combination (and `nhl-api-py` does not expose
  them either). Wrappers are kept for when the endpoints are
  re-enabled; tests use a NULL-tolerant assertion pattern.

## New Files

| Path | Purpose |
|------|---------|
| `R/helpers_nhl_edge.R` | `.nhl_edge_api()` + `.nhl_edge_to_df()` |
| `R/helpers_nhl_records.R` | `.nhl_records_api()` |
| `R/nhl_edge_*.R` (33 files) | NHL Edge analytics wrappers |
| `R/nhl_cat_edge_*.R` (2 files) | NHL Edge CAT-framework variants |
| `R/nhl_records_*.R` (25 files) | NHL Records API wrappers |
| `R/nhl_stats_franchise.R`, `R/nhl_stats_players.R`, ... (13 files) | NHL Stats REST dedicated wrappers |
| `R/nhl_wsc_pbp.R`, `R/nhl_draft_tracker.R`, `R/nhl_ppt_replay.R`, `R/nhl_ppt_replay_goal.R`, `R/nhl_postal_lookup.R`, `R/nhl_smartlinks.R` | api-web misc endpoints |
| `R/nhl_game_ids_by_season.R`, `R/nhl_all_players_by_season.R`, `R/nhl_player_career_stats.R`, `R/nhl_team_summary_range.R`, `R/nhl_skater_summary_range.R`, `R/nhl_goalie_summary_range.R` | Helper aggregators |
| `tests/testthat/test-nhl_edge_*.R` (35 files) | Tests for Edge family |
| `tests/testthat/test-nhl_records_*.R` (25 files) | Tests for Records family |
| `tests/testthat/test-nhl_stats_*.R` (13 new files) | Tests for new Stats REST wrappers |
| `tests/testthat/test-nhl_wsc_pbp.R`, ... (6 misc files) | Tests for api-web misc additions |
| `tests/testthat/test-nhl_*_summary_range.R`, ... (6 helper files) | Tests for helper aggregators |
| `man/*.Rd` (86 new) | Generated roxygen2 docs |
| `data-raw/nhl_missing_endpoint_function_mapping.md` | Endpoint→function mapping table |
| `data-raw/nhl_api_web_openapi.{json,yaml}` | OpenAPI 3.0.3 spec for `api-web.nhle.com/v1/` (132 endpoints) |
| `data-raw/nhl_stats_rest_openapi.{json,yaml}` | OpenAPI 3.0.3 spec for `api.nhle.com/stats/rest/{lang}/` (21 endpoints) |
| `data-raw/nhl_records_openapi.{json,yaml}` | OpenAPI 3.0.3 spec for `records.nhl.com/site/api/` (442 endpoints) |
| `data-raw/_gen_openapi.py` | Regenerator script (reads `_endpoints_catalog.json`) |
| `data-raw/_endpoints_catalog.json` | Cached upstream endpoint catalog from `dfleis/nhl-api-docs` |

## Modified Files

| Path | Change |
|------|--------|
| `R/nhl_stats_misc.R` | Roxygen now enumerates every valid `endpoint` value (was previously listing only 4 of ~17) |
| `R/nhl_scoreboard.R` | Added optional `date` argument |
| `R/nhl_meta.R` | Added `year` / `series_letter` for playoff-series metadata |
| `R/nhl_draft_year.R` | Added `/draft/picks/{year}/all` shortcut for `round = NULL`/`"all"` |
| `NEWS.md` | New 1.0.0 (continued development) section |
| `cran-comments.md` | New release summary covering all NHL endpoint families |
| `README.Rmd` / `README.md` | New "Key Features" entries + Quick Start examples |
| `CLAUDE.md` | New "NHL Endpoint Families" section, API backends table, conventional commit scopes |
| `.github/copilot-instructions.md` | File listings for `nhl_edge_*`, `nhl_records_*`, `nhl_stats_*` families; API backends table; new commit scopes |
| `_pkgdown.yml` | New reference sections for Edge skater/goalie/team, Records, Stats REST dedicated wrappers, helper aggregators, and api-web misc |
| `NAMESPACE` | Regenerated: 132 → 218 exports (+86) |

## Verification

* `devtools::check()` -- 0 errors, 0 warnings, 0 notes
* `devtools::test()` (`RUN_NHL_TESTS=true`) -- 811 PASS, 35 SKIP, 1 FAIL
  (the 1 failure is the pre-existing `test-load_nhl.R:48` for
  `load_nhl_team_box(seasons = 2021)`, unrelated to this work)

## Method

API catalogs were pulled from `dfleis/nhl-api-docs/data/final/ENDPOINTS.json`
(595 endpoints across 3 base URLs) and cross-referenced against the
`coreyjs/nhl-api-py` source modules. fastRhockey URL coverage was
extracted by grep on every `https://api*nhle.com` and `records.nhl.com`
literal in `R/`. Set diffs are exact, not approximate. The endpoint
mapping doc and OpenAPI specs in `data-raw/` are reproducible: re-run
`Rscript data-raw/_gen_openapi.py` to refresh from the upstream catalog.

---

# PR Development Notes (v1.0.0 - PWHL Expansion + Code Quality)

85 files changed, +7 531, −4 878

## Summary

Expand PWHL coverage to all documented HockeyTech API endpoints per the
[PWHL-Data-Reference](https://github.com/sportsdataverse/PWHL-Data-Reference).
Remove `globalVariables()` in favour of proper `.data$` masking and string-based
tidy selection. Fix pre-existing R CMD check notes. Result: **0 errors, 0
warnings, 0 notes**.

## Commits (branch: `pwhl-expansion`)

```
3990c09 docs: update package metadata, README, and generated man pages
0890651 fix: resolve pre-existing R CMD check notes
eb9b8e3 refactor: replace globalVariables with .data$ masking
aa67cab feat(pwhl): add 16 new PWHL endpoints, loaders, and helpers
```

## New PWHL Functions (16)

| Function | Endpoint Feed | Description |
|----------|--------------|-------------|
| `pwhl_player_info()` | modulekit | Player biographical profile |
| `pwhl_player_game_log()` | modulekit | Per-game stats for a player/season |
| `pwhl_player_stats()` | modulekit | Career/season-by-season stats |
| `pwhl_player_search()` | modulekit | Search players by name |
| `pwhl_leaders()` | modulekit | Top scorers / top goalies |
| `pwhl_transactions()` | modulekit | Season transactions |
| `pwhl_streaks()` | modulekit | Player streaks |
| `pwhl_playoff_bracket()` | modulekit | Playoff bracket / series data |
| `pwhl_game_summary()` | gc | Detailed game summary (goals, penalties, shots, three stars) |
| `pwhl_scorebar()` | modulekit | Recent / upcoming game scores |
| `most_recent_pwhl_season()` | (computed) | Current PWHL season year utility |
| `load_pwhl_pbp()` | data release | Pre-scraped play-by-play loader |
| `load_pwhl_player_box()` | data release | Pre-scraped player box scores loader |
| `load_pwhl_schedule()` | data release | Pre-scraped schedules loader |
| `load_pwhl_rosters()` | data release | Pre-scraped rosters loader |
| `update_pwhl_db()` | data release | Create/update PWHL PBP database |

## Updated Functions

| Function | Change |
|----------|--------|
| `pwhl_season_id()` | Now API-driven via modulekit `view=seasons`, with hardcoded fallback |

## Refactoring

* Removed `utils::globalVariables()` (~90 entries)
* All bare NSE column references replaced with `.data$` pronoun or string-based selection
* Replaced deprecated `dplyr::mutate_at()` with `dplyr::across()`
* Replaced deprecated `.data$` in tidyselect contexts (`select`, `rename`) with strings / `all_of()`
* Fixed `\itemize{\item{}{}}` Rd syntax → markdown bullet lists
* Silenced `packageStartupMessage()` in `.onLoad()` per CRAN policy

## New Files

| Path | Purpose |
|------|---------|
| `R/pwhl_helpers.R` | `.pwhl_api()`, `.pwhl_modulekit_url()`, `.pwhl_gc_url()`, `.pwhl_resolve_season_id()` |
| `R/pwhl_loaders.R` | `load_pwhl_*()`, `update_pwhl_db()`, `build_pwhl_db()`, `get_missing_pwhl_games()` |
| `R/pwhl_player_info.R` | `pwhl_player_info()` |
| `R/pwhl_player_game_log.R` | `pwhl_player_game_log()` |
| `R/pwhl_player_stats.R` | `pwhl_player_stats()` |
| `R/pwhl_player_search.R` | `pwhl_player_search()` |
| `R/pwhl_leaders.R` | `pwhl_leaders()` |
| `R/pwhl_transactions.R` | `pwhl_transactions()` |
| `R/pwhl_streaks.R` | `pwhl_streaks()` |
| `R/pwhl_playoff_bracket.R` | `pwhl_playoff_bracket()` |
| `R/pwhl_game_summary.R` | `pwhl_game_summary()` |
| `R/pwhl_scorebar.R` | `pwhl_scorebar()` |
| `cran_comments.md` | CRAN submission notes |
| 14 test files | Tests for all new endpoints |
| 12 man pages | Generated roxygen2 docs |

## Modified Files

| Path | Change |
|------|--------|
| `R/utils.R` | Added `most_recent_pwhl_season()`, removed `globalVariables()` |
| `R/pwhl_season_id.R` | Rewritten: API-driven with fallback |
| `R/helpers_nhl.R` | `.data$` masking, string-based select/pivot_wider |
| `R/nhl_game_feed.R` | `.data$` masking, fix `season_type` local-vs-column, `all_of()` rename |
| `R/nhl_conferences.R` | `.data$` in `distinct()` |
| `R/nhl_divisions.R` | `.data$` in `distinct()` |
| `R/pwhl_pbp.R` | `.data$` masking in penalty tracking, init `game_df` before tryCatch |
| `R/pwhl_player_box.R` | `across()` replacing `mutate_at()`, `.data$` masking |
| `R/pwhl_schedule.R` | `.data$` in filter, `!!` for env vars |
| `R/pwhl_game_info.R` | `.data$` in filter, string-based `pull()` |
| `R/pwhl_team_roster.R` | `.data$` in mutate, string-based `relocate()`/`separate()` |
| `R/pwhl_stat_leaders.R` | String-based `separate()` |
| `R/pwhl_standings.R` | Added `@examples`, expanded `@return` |
| `R/pwhl_teams.R` | Expanded `@return` with column docs |
| `R/zzz.R` | Silenced `packageStartupMessage()` in `.onLoad()` |
| `R/nhl_pbp.R` | `\itemize` → markdown |
| `R/phf_pbp.R` | `\itemize` → markdown |
| `R/data.R` | `\describe` → markdown |
| `NEWS.md` | New PWHL functions + improvements |
| `README.Rmd` / `README.md` | PWHL features + quick-start examples |
| `CLAUDE.md` | PWHL function table, API backend docs |
| `.github/copilot-instructions.md` | New file listings, API backends, season format |
| `_pkgdown.yml` | Organized PWHL reference sections + loader section |
| `.Rbuildignore` | Added `cran_comments.md` |

## Checklist

- [x] `devtools::document()` — regenerate NAMESPACE and man pages
- [x] `devtools::check()` — 0 errors, 0 warnings, 0 notes
- [ ] Verify `pwhl_season_id()` returns dynamic data
- [ ] Verify new modulekit functions parse JSONP correctly
- [ ] Spot-check: `pwhl_player_info(28)`, `pwhl_leaders("skaters", 2025)`, `pwhl_game_summary(27)`
- [ ] Build pkgdown site locally: `pkgdown::build_site()`
- [ ] Knit README.Rmd to README.md
