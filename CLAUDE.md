# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

fastRhockey is an R package for accessing hockey data from the **NHL** and **PWHL** via public web APIs. The package also contains deprecated functions for the defunct **PHF** league. Part of the [SportsDataverse](https://sportsdataverse.org/) family.

**R >= 4.0.0** | **License: MIT** | **testthat edition 3** | **Version: 1.0.0**

## Common Commands

```bash
# Install dependencies
devtools::install_deps(dependencies = TRUE)

# Build documentation (roxygen2)
devtools::document()

# Run full R CMD check
devtools::check()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-nhl_standings.R")

# Build and install locally
devtools::install()

# Build pkgdown site
pkgdown::build_site()
```

## Architecture

```
R/                          # Source code (one function per file)
  utils.R                   # Shared helpers: check_status, most_recent_*_season, messaging
  zzz.R                     # .onLoad() — xG model download/caching lifecycle
  fastRhockey-package.R     # Package-level roxygen2 documentation, lifecycle import
  helpers_nhl.R             # helper_nhl_calculate_xg(), helper_nhl_prepare_xg_data()
  helpers_nhl_edge.R        # .nhl_edge_api(), .nhl_edge_to_df() — NHL Edge URL builder
  helpers_nhl_records.R     # .nhl_records_api() — records.nhl.com URL builder
  pwhl_helpers.R            # .pwhl_api(), .pwhl_modulekit_url(), .pwhl_gc_url() — PWHL JSONP helpers
  nhl_*.R                   # NHL functions (api-web.nhle.com + api.nhle.com/stats)
  nhl_edge_*.R              # NHL Edge analytics (api-web.nhle.com/v1/edge/...)
  nhl_cat_edge_*.R          # NHL Edge "CAT" variants (api-web.nhle.com/v1/cat/edge/...)
  nhl_records_*.R           # NHL records API (records.nhl.com/site/api/...)
  phf_*.R                   # PHF functions (deprecated — league defunct)
  pwhl_*.R                  # PWHL functions (lscluster.hockeytech.com)

tests/testthat/             # Unit tests (testthat edition 3)
man/                        # Auto-generated roxygen2 documentation
data/                       # Included datasets (.rda)
data-raw/                   # Scripts for generating package data (*.md files untracked)
                            # Includes nhl_missing_endpoint_function_mapping.md
                            # and OpenAPI 3.0.3 specs (nhl_*_openapi.{json,yaml})
                            # for api-web, stats-rest, and records backends.
vignettes/                  # Vignettes for pkgdown site
```

### API Backends

| Backend | Base URL | Used By | Auth |
|---------|----------|---------|------|
| NHL Web API | `api-web.nhle.com/v1/` | Game feed, schedule, standings, rosters, gamecenter, draft, scoreboard, scores, meta, location, partner-game, where-to-watch, smartlinks, postal-lookup, ppt-replay, wsc | None |
| NHL Edge API | `api-web.nhle.com/v1/edge/...` and `v1/cat/edge/...` | Skater/goalie/team Edge advanced metrics (shot location, shot speed, skating speed, skating distance, zone time, comparisons, top-10 leaderboards) | None |
| NHL Stats API | `api.nhle.com/stats/rest/{lang}/` | Skater/goalie/team stats, draft, franchise, players, glossary, country, config, leaders, milestones | None |
| NHL Records API | `records.nhl.com/site/api/` | Franchise totals, player/skater/goalie career and real-time stats, draft lottery, hall of fame, trophies, awards, attendance, venues, officials, combine | None |
| HockeyTech (statviewfeed) | `lscluster.hockeytech.com/feed/?feed=statviewfeed` | PWHL schedule, standings, roster, PBP, box scores, stats | Public key `694cfeed58c932ee` |
| HockeyTech (modulekit) | `lscluster.hockeytech.com/feed/?feed=modulekit` | PWHL seasons, player info, leaders, transactions, streaks, brackets, scorebar | Public key `446521baf8c38984` |
| HockeyTech (gc) | `lscluster.hockeytech.com/feed/?feed=gc` | PWHL game summary, game center | Public key `446521baf8c38984` |

- **NHL Web API** returns clean JSON parsed with `jsonlite::fromJSON()`
- **NHL Edge API** uses an internal `.nhl_edge_api(base, season, game_type, prefix)` helper that handles the `/now` vs `/{season}/{gameType}` URL split, and `.nhl_edge_to_df()` to normalize the assorted response shapes
- **NHL Stats API** uses Cayenne filter expressions for query params. Note: the `leaders/{skaters,goalies}/{attribute}` endpoint does **not** accept `start`/`limit` query parameters (returns 500); valid goalie attributes are restricted to `savePctg`, `gaa`, and `shutouts`
- **NHL Records API** uses an internal `.nhl_records_api(resource, cayenne_exp, sort, limit, start, query)` helper. The records API does **not** support path-suffix filtering (`franchise/{id}` returns 404); use `cayenneExp` filtering instead
- **HockeyTech** returns JSONP with Angular callbacks that must be regex-stripped before parsing
- PWHL internal helpers in `pwhl_helpers.R`: `.pwhl_api()` strips JSONP, `.pwhl_modulekit_url()` / `.pwhl_gc_url()` build URLs

### PWHL Functions (35 exported)

| Function | Category | Endpoint Feed |
|----------|----------|--------------|
| `pwhl_teams()` | Teams | statviewfeed |
| `pwhl_team_roster()` | Teams | statviewfeed |
| `pwhl_schedule()` | Games | statviewfeed |
| `pwhl_standings()` | Teams | statviewfeed |
| `pwhl_game_info()` | Games | statviewfeed |
| `pwhl_pbp()` | Games | statviewfeed |
| `pwhl_player_box()` | Games | statviewfeed |
| `pwhl_stats()` | Players | statviewfeed |
| `pwhl_season_id()` | League | modulekit (dynamic, with hardcoded fallback) |
| `pwhl_player_info()` | Players | modulekit |
| `pwhl_player_game_log()` | Players | modulekit |
| `pwhl_player_stats()` | Players | modulekit |
| `pwhl_player_search()` | Players | modulekit |
| `pwhl_leaders()` | Players | modulekit |
| `pwhl_transactions()` | League | modulekit |
| `pwhl_streaks()` | Players | modulekit |
| `pwhl_playoff_bracket()` | League | modulekit |
| `pwhl_scorebar()` | Games | modulekit |
| `pwhl_game_summary()` | Games | gc |
| `most_recent_pwhl_season()` | Utility | (computed) |
| `load_pwhl_pbp()` | Loader | sportsdataverse-data releases |
| `load_pwhl_player_box()` | Loader | sportsdataverse-data releases |
| `load_pwhl_skater_box()` | Loader | sportsdataverse-data releases |
| `load_pwhl_goalie_box()` | Loader | sportsdataverse-data releases |
| `load_pwhl_team_box()` | Loader | sportsdataverse-data releases |
| `load_pwhl_schedule()` | Loader | sportsdataverse-data releases |
| `load_pwhl_rosters()` | Loader | sportsdataverse-data releases |
| `load_pwhl_game_rosters()` | Loader | sportsdataverse-data releases |
| `load_pwhl_game_info()` | Loader | sportsdataverse-data releases |
| `load_pwhl_scoring_summary()` | Loader | sportsdataverse-data releases |
| `load_pwhl_penalty_summary()` | Loader | sportsdataverse-data releases |
| `load_pwhl_three_stars()` | Loader | sportsdataverse-data releases |
| `load_pwhl_officials()` | Loader | sportsdataverse-data releases |
| `load_pwhl_shots_by_period()` | Loader | sportsdataverse-data releases |
| `load_pwhl_shootout()` | Loader | sportsdataverse-data releases |
| `update_pwhl_db()` | Loader | sportsdataverse-data releases |

All `load_pwhl_*()` helpers share `.pwhl_release_loader()` (in `pwhl_loaders.R`),
which validates seasons, builds release URLs from a `(release_tag, file_prefix)`
pair, downloads in parallel with optional `progressr`, optionally writes into a
`DBIConnection`, and tags output with the `fastRhockey_data` S3 class. Adding a
new dataset = one row in the catalog table at the top of `pwhl_loaders.R` plus
a thin exported wrapper.

### NHL Endpoint Families (extended in 1.0.0)

| Family | Count | Files | Helper |
|--------|------:|-------|--------|
| NHL Edge analytics | 33 | `nhl_edge_*.R`, `nhl_cat_edge_*.R` | `.nhl_edge_api()` + `.nhl_edge_to_df()` in `helpers_nhl_edge.R` |
| NHL Records API | 25 | `nhl_records_*.R` | `.nhl_records_api()` in `helpers_nhl_records.R` |
| NHL Stats REST (dedicated wrappers) | 13 | `nhl_stats_franchise.R`, `nhl_stats_players.R`, `nhl_stats_glossary.R`, `nhl_stats_country.R`, `nhl_stats_config.R`, `nhl_stats_ping.R`, `nhl_stats_skater_leaders.R`, `nhl_stats_goalie_leaders.R`, `nhl_stats_skater_milestones.R`, `nhl_stats_goalie_milestones.R`, `nhl_stats_team_listing.R`, `nhl_stats_game_listing.R`, `nhl_stats_content_module.R` | (none — uses `httr::RETRY` directly) |
| NHL api-web miscellaneous | 6 | `nhl_wsc_pbp.R`, `nhl_draft_tracker.R`, `nhl_ppt_replay.R`, `nhl_ppt_replay_goal.R`, `nhl_postal_lookup.R`, `nhl_smartlinks.R` | (none) |
| NHL helper aggregators | 6 | `nhl_game_ids_by_season.R`, `nhl_all_players_by_season.R`, `nhl_player_career_stats.R`, `nhl_team_summary_range.R`, `nhl_skater_summary_range.R`, `nhl_goalie_summary_range.R` | (none — orchestrates other exported functions) |

The Edge family pairs `/now` and `/{season}/{gameType}` URLs behind a
single wrapper that takes an optional `season` argument (default `NULL`
→ current season). The Records family parallels the Stats REST shape:
all 442 documented endpoints share `https://records.nhl.com/site/api/{resource}`
with optional `cayenneExp` filtering and `limit`/`start` pagination,
and the response shape is `{data: [...], total: N}` for tabular
endpoints.

Endpoint catalog and OpenAPI 3.0.3 specs for all three NHL backends are
in `data-raw/nhl_*_openapi.{json,yaml}`. The function-to-endpoint
mapping is in `data-raw/nhl_missing_endpoint_function_mapping.md`.
Both are regenerated by `data-raw/_gen_openapi.py` from the upstream
[dfleis/nhl-api-docs](https://github.com/dfleis/nhl-api-docs) catalog.

### NHL Loaders (16 exported + DB helpers)

| Function | Category | Release Tag |
|----------|----------|-------------|
| `load_nhl_pbp()` | Loader | nhl_pbp_full |
| `load_nhl_pbp_lite()` | Loader | nhl_pbp_lite |
| `load_nhl_player_box()` | Loader | nhl_player_boxscores |
| `load_nhl_skater_box()` | Loader | nhl_skater_boxscores |
| `load_nhl_goalie_box()` | Loader | nhl_goalie_boxscores |
| `load_nhl_team_box()` | Loader | nhl_team_boxscores |
| `load_nhl_schedule()` | Loader | nhl_schedules |
| `load_nhl_rosters()` | Loader | nhl_rosters |
| `load_nhl_game_rosters()` | Loader | nhl_game_rosters |
| `load_nhl_game_info()` | Loader | nhl_game_info |
| `load_nhl_scoring()` | Loader | nhl_scoring |
| `load_nhl_penalties()` | Loader | nhl_penalties |
| `load_nhl_three_stars()` | Loader | nhl_three_stars |
| `load_nhl_scratches()` | Loader | nhl_scratches |
| `load_nhl_linescore()` | Loader | nhl_linescore |
| `load_nhl_shifts()` | Loader | nhl_shifts |
| `update_nhl_db()` | DB | (uses load_nhl_pbp) |

All `load_nhl_*()` helpers share `.nhl_release_loader()` (in `nhl_pbp.R`),
which validates seasons (min 2011), builds release URLs from
`(release_tag, file_prefix)`, downloads in parallel with optional `progressr`,
optionally writes into a `DBIConnection`, and tags output with the
`fastRhockey_data` S3 class.

### S3 Class: `fastRhockey_data`

All data-returning functions wrap output in `fastRhockey_data` via `make_fastRhockey_data()`. Tests should check: `expect_s3_class(x, "fastRhockey_data")`.

### xG Model Pipeline

Three XGBoost models (5v5, special teams, penalty shots) downloaded on `.onLoad()` from `sportsdataverse/fastRhockey-nhl-data/main/models/`, cached in `tools::R_user_dir("fastRhockey", "cache")`. Requires the `xgboost` suggested package. Models are stored in `.xg_env` (a package-level environment in `zzz.R`) and accessed via `.xg_env$xg_model_5v5` etc. in `helpers_nhl.R`.

### Deprecation Strategy

The package uses `lifecycle` for formal deprecation of PHF functions:
- **PHF functions** (21 functions): Deprecated in v1.0.0 — league ceased operations
- All deprecated functions emit `lifecycle::deprecate_stop()` and raise errors
- Tests for deprecated functions check for `lifecycle_error_deprecated` class

## Key Conventions

- One exported function per file; filename matches function name
- `httr::RETRY("GET", ...)` for all HTTP requests
- `janitor::clean_names()` on all API responses
- `tryCatch` with `message()` for errors — functions return `NULL` on failure, not errors
- `glue::glue()` for URL string interpolation
- Internal helpers prefixed with `.` (e.g., `.parse_game_rosters()`, `.pwhl_api()`)
- `globalVariables()` in `utils.R` suppress R CMD check NSE notes
- Conventional Commits: `<type>(<scope>): <description>`
  - Types: feat, fix, docs, style, refactor, test, build, ci, chore
  - Scopes: nhl, nhl-edge, nhl-records, nhl-stats, phf, pwhl, xg, pkgdown, ci, loader

### Naming Conventions

- **Exported:** `league_entity_action()` (e.g., `nhl_teams_roster()`, `pwhl_schedule()`)
- **Internal:** `.snake_case()` with leading dot (e.g., `.parse_game_rosters()`, `.pwhl_api()`)
- **Tests:** `test-function_name.R`
- **Parameters:** `snake_case` (e.g., `game_id`, `team_abbr`, `season`)
- **Datasets:** `snake_case` .rda files

### Season Formats

- NHL Web API / Stats API: `"20242025"` (concatenated years)
- `most_recent_nhl_season()` returns end year as numeric (e.g., `2025`)
- `most_recent_nhl_season_api_param()` returns `"20242025"` format
- PWHL: numeric year (e.g., `2025`)
- `most_recent_pwhl_season()` returns concluding year as numeric (e.g., `2025`)
- `pwhl_season_id()` maps (season, game_type) → HockeyTech `season_id`
- `.pwhl_resolve_season_id(season, game_type)` is the internal convenience wrapper

## Testing

- **Framework:** testthat edition 3
- **Run all:** `devtools::test()`
- **Environment:** Set `NOT_CRAN=true` to run full test suite locally
- **Skip helpers:** `tests/testthat/helper-skip.R` provides `skip_nhl_test()`, `skip_phf_test()`, `skip_pwhl_test()` controlled by env vars `RUN_NHL_TESTS`, `RUN_PHF_TESTS`, `RUN_PWHL_TESTS` (NHL/PWHL default to `true`, PHF to `false`). CI workflow uses the same variable names.
- **Pattern:** `skip_on_cran()` + `skip_*_test()`, check `data.frame` + `fastRhockey_data` classes, validate `nrow(x) > 0`, check expected column names
- **xG tests:** additionally require `skip_if_not_installed("xgboost")`
- **Deprecated functions:** wrap calls in `suppressWarnings()` in tests
- **Reproducibility:** use specific game IDs/dates (e.g., `game_id = 2023020001`, PWHL `game_id = 27`)

## Commit Authorship

- **Never** include AI tools (Claude, Copilot, ChatGPT, etc.) as a co-author or author in commit messages. Commits should be attributed solely to the human contributors who made or directed the changes.

## Dependencies

**Imports:** cli, data.table, dplyr, glue, httr, janitor, jsonlite, lifecycle, lubridate, magrittr, purrr, Rcpp, RcppParallel, rlang, rvest, stringr, tibble, tidyr

**Suggests:** crayon, curl, DBI, furrr, future, ggplot2, ggrepel, progressr, rmarkdown, RSQLite, stringi, stats, testthat (>= 3.0.0), usethis, xgboost, xml2
