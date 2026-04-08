# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project Overview

fastRhockey is an R package for accessing hockey data from the **NHL**
and **PWHL** via public web APIs. The package also contains deprecated
functions for the defunct **PHF** league. Part of the
[SportsDataverse](https://sportsdataverse.org/) family.

**R \>= 4.0.0** \| **License: MIT** \| **testthat edition 3** \|
**Version: 1.0.0**

## Common Commands

``` bash
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

    R/                          # Source code (one function per file)
      utils.R                   # Shared helpers: check_status, most_recent_*_season, messaging
      zzz.R                     # .onLoad() — xG model download/caching lifecycle
      fastRhockey-package.R     # Package-level roxygen2 documentation, lifecycle import
      helpers_nhl.R             # helper_nhl_calculate_xg(), helper_nhl_prepare_xg_data()
      pwhl_helpers.R            # .pwhl_api(), .pwhl_modulekit_url(), .pwhl_gc_url() — PWHL JSONP helpers
      nhl_*.R                   # NHL functions (api-web.nhle.com + api.nhle.com/stats)
      phf_*.R                   # PHF functions (deprecated — league defunct)
      pwhl_*.R                  # PWHL functions (lscluster.hockeytech.com)

    tests/testthat/             # Unit tests (testthat edition 3)
    man/                        # Auto-generated roxygen2 documentation
    data/                       # Included datasets (.rda)
    data-raw/                   # Scripts for generating package data (*.md files untracked)
    vignettes/                  # Vignettes for pkgdown site

### API Backends

| Backend                   | Base URL                                           | Used By                                                                       | Auth                          |
|---------------------------|----------------------------------------------------|-------------------------------------------------------------------------------|-------------------------------|
| NHL Web API               | `api-web.nhle.com/v1/`                             | Game feed, schedule, standings, rosters                                       | None                          |
| NHL Stats API             | `api.nhle.com/stats/rest/{lang}/`                  | Skater/goalie/team stats, draft                                               | None                          |
| HockeyTech (statviewfeed) | `lscluster.hockeytech.com/feed/?feed=statviewfeed` | PWHL schedule, standings, roster, PBP, box scores, stats                      | Public key `694cfeed58c932ee` |
| HockeyTech (modulekit)    | `lscluster.hockeytech.com/feed/?feed=modulekit`    | PWHL seasons, player info, leaders, transactions, streaks, brackets, scorebar | Public key `446521baf8c38984` |
| HockeyTech (gc)           | `lscluster.hockeytech.com/feed/?feed=gc`           | PWHL game summary, game center                                                | Public key `446521baf8c38984` |

- **NHL Web API** returns clean JSON parsed with
  [`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
- **NHL Stats API** uses Cayenne filter expressions for query params
- **HockeyTech** returns JSONP with Angular callbacks that must be
  regex-stripped before parsing
- PWHL internal helpers in `pwhl_helpers.R`: `.pwhl_api()` strips JSONP,
  `.pwhl_modulekit_url()` / `.pwhl_gc_url()` build URLs

### PWHL Functions (20 exported)

| Function                                                                                                    | Category | Endpoint Feed                                |
|-------------------------------------------------------------------------------------------------------------|----------|----------------------------------------------|
| [`pwhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_teams.md)                           | Teams    | statviewfeed                                 |
| [`pwhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_team_roster.md)               | Teams    | statviewfeed                                 |
| [`pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_schedule.md)                     | Games    | statviewfeed                                 |
| [`pwhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_standings.md)                   | Teams    | statviewfeed                                 |
| [`pwhl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_game_info.md)                   | Games    | statviewfeed                                 |
| [`pwhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_pbp.md)                               | Games    | statviewfeed                                 |
| [`pwhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_box.md)                 | Games    | statviewfeed                                 |
| [`pwhl_stats()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_stats.md)                           | Players  | statviewfeed                                 |
| [`pwhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_season_id.md)                   | League   | modulekit (dynamic, with hardcoded fallback) |
| [`pwhl_player_info()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_info.md)               | Players  | modulekit                                    |
| [`pwhl_player_game_log()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_game_log.md)       | Players  | modulekit                                    |
| [`pwhl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_stats.md)             | Players  | modulekit                                    |
| [`pwhl_player_search()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_search.md)           | Players  | modulekit                                    |
| [`pwhl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_leaders.md)                       | Players  | modulekit                                    |
| [`pwhl_transactions()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_transactions.md)             | League   | modulekit                                    |
| [`pwhl_streaks()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_streaks.md)                       | Players  | modulekit                                    |
| [`pwhl_playoff_bracket()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_playoff_bracket.md)       | League   | modulekit                                    |
| [`pwhl_scorebar()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_scorebar.md)                     | Games    | modulekit                                    |
| [`pwhl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_game_summary.md)             | Games    | gc                                           |
| [`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md) | Utility  | (computed)                                   |
| [`load_pwhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_pbp.md)                     | Loader   | sportsdataverse-data releases                |
| [`load_pwhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_player_box.md)       | Loader   | sportsdataverse-data releases                |
| [`load_pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_schedule.md)           | Loader   | sportsdataverse-data releases                |
| [`load_pwhl_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_rosters.md)             | Loader   | sportsdataverse-data releases                |
| [`update_pwhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_pwhl_db.md)                   | Loader   | sportsdataverse-data releases                |

### S3 Class: `fastRhockey_data`

All data-returning functions wrap output in `fastRhockey_data` via
`make_fastRhockey_data()`. Tests should check:
`expect_s3_class(x, "fastRhockey_data")`.

### xG Model Pipeline

Three XGBoost models (5v5, special teams, penalty shots) downloaded on
`.onLoad()` from `sportsdataverse/fastRhockey-nhl-data/main/models/`,
cached in `tools::R_user_dir("fastRhockey", "cache")`. Requires the
`xgboost` suggested package. Models are stored in `.xg_env` (a
package-level environment in `zzz.R`) and accessed via
`.xg_env$xg_model_5v5` etc. in `helpers_nhl.R`.

### Deprecation Strategy

The package uses `lifecycle` for formal deprecation of PHF functions: -
**PHF functions** (21 functions): Deprecated in v1.0.0 — league ceased
operations - All deprecated functions emit
[`lifecycle::deprecate_stop()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html)
and raise errors - Tests for deprecated functions check for
`lifecycle_error_deprecated` class

## Key Conventions

- One exported function per file; filename matches function name
- `httr::RETRY("GET", ...)` for all HTTP requests
- [`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html)
  on all API responses
- `tryCatch` with [`message()`](https://rdrr.io/r/base/message.html) for
  errors — functions return `NULL` on failure, not errors
- [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) for
  URL string interpolation
- Internal helpers prefixed with `.` (e.g.,
  [`.parse_game_rosters()`](https://fastRhockey.sportsdataverse.org/reference/dot-parse_game_rosters.md),
  `.pwhl_api()`)
- [`globalVariables()`](https://rdrr.io/r/utils/globalVariables.html) in
  `utils.R` suppress R CMD check NSE notes
- Conventional Commits: `<type>(<scope>): <description>`
  - Types: feat, fix, docs, style, refactor, test, build, ci, chore
  - Scopes: nhl, phf, pwhl, xg, pkgdown, ci, loader

### Naming Conventions

- **Exported:** `league_entity_action()` (e.g.,
  [`nhl_teams_roster()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams_roster.md),
  [`pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_schedule.md))
- **Internal:** `.snake_case()` with leading dot (e.g.,
  [`.parse_game_rosters()`](https://fastRhockey.sportsdataverse.org/reference/dot-parse_game_rosters.md),
  `.pwhl_api()`)
- **Tests:** `test-function_name.R`
- **Parameters:** `snake_case` (e.g., `game_id`, `team_abbr`, `season`)
- **Datasets:** `snake_case` .rda files

### Season Formats

- NHL Web API / Stats API: `"20242025"` (concatenated years)
- [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md)
  returns end year as numeric (e.g., `2025`)
- [`most_recent_nhl_season_api_param()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season_api_param.md)
  returns `"20242025"` format
- PWHL: numeric year (e.g., `2025`)
- [`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md)
  returns concluding year as numeric (e.g., `2025`)
- [`pwhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_season_id.md)
  maps (season, game_type) → HockeyTech `season_id`
- `.pwhl_resolve_season_id(season, game_type)` is the internal
  convenience wrapper

## Testing

- **Framework:** testthat edition 3
- **Run all:** `devtools::test()`
- **Environment:** Set `NOT_CRAN=true` to run full test suite locally
- **Skip helpers:** `tests/testthat/helper-skip.R` provides
  `skip_nhl_test()`, `skip_phf_test()`, `skip_pwhl_test()` controlled by
  env vars `RUN_NHL_TESTS`, `RUN_PHF_TESTS`, `RUN_PWHL_TESTS` (NHL/PWHL
  default to `true`, PHF to `false`). CI workflow uses the same variable
  names.
- **Pattern:** `skip_on_cran()` + `skip_*_test()`, check `data.frame` +
  `fastRhockey_data` classes, validate `nrow(x) > 0`, check expected
  column names
- **xG tests:** additionally require `skip_if_not_installed("xgboost")`
- **Deprecated functions:** wrap calls in
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html) in tests
- **Reproducibility:** use specific game IDs/dates (e.g.,
  `game_id = 2023020001`, PWHL `game_id = 27`)

## Commit Authorship

- **Never** include AI tools (Claude, Copilot, ChatGPT, etc.) as a
  co-author or author in commit messages. Commits should be attributed
  solely to the human contributors who made or directed the changes.

## Dependencies

**Imports:** cli, data.table, dplyr, glue, httr, janitor, jsonlite,
lifecycle, lubridate, magrittr, purrr, Rcpp, RcppParallel, rlang, rvest,
stringr, tibble, tidyr

**Suggests:** crayon, curl, DBI, furrr, future, ggplot2, ggrepel,
progressr, rmarkdown, RSQLite, stringi, stats, testthat (\>= 3.0.0),
usethis, xgboost, xml2
