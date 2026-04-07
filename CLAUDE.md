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
  nhl_*.R                   # NHL functions (api-web.nhle.com + api.nhle.com/stats)
  phf_*.R                   # PHF functions (deprecated — league defunct)
  pwhl_*.R                  # PWHL functions (lscluster.hockeytech.com)

tests/testthat/             # Unit tests (testthat edition 3)
man/                        # Auto-generated roxygen2 documentation
data/                       # Included datasets (.rda)
data-raw/                   # Scripts for generating package data (*.md files untracked)
vignettes/                  # Vignettes for pkgdown site
```

### API Backends

| Backend | Base URL | Used By | Auth |
|---------|----------|---------|------|
| NHL Web API | `api-web.nhle.com/v1/` | Game feed, schedule, standings, rosters | None |
| NHL Stats API | `api.nhle.com/stats/rest/{lang}/` | Skater/goalie/team stats, draft | None |
| HockeyTech | `lscluster.hockeytech.com/feed/` | All PWHL functions | Public key in URL |

- **NHL Web API** returns clean JSON parsed with `jsonlite::fromJSON()`
- **NHL Stats API** uses Cayenne filter expressions for query params
- **HockeyTech** returns JSONP with Angular callbacks that must be regex-stripped before parsing

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
- Internal helpers prefixed with `.` (e.g., `.parse_game_rosters()`)
- `globalVariables()` in `utils.R` suppress R CMD check NSE notes
- Conventional Commits: `<type>(<scope>): <description>`
  - Types: feat, fix, docs, style, refactor, test, build, ci, chore
  - Scopes: nhl, phf, pwhl, xg, pkgdown, ci, loader

### Naming Conventions

- **Exported:** `league_entity_action()` (e.g., `nhl_teams_roster()`, `pwhl_schedule()`)
- **Internal:** `.snake_case()` with leading dot (e.g., `.parse_game_rosters()`)
- **Tests:** `test-function_name.R`
- **Parameters:** `snake_case` (e.g., `game_id`, `team_abbr`, `season`)
- **Datasets:** `snake_case` .rda files

### Season Formats

- NHL Web API / Stats API: `"20242025"` (concatenated years)
- `most_recent_nhl_season()` returns end year as numeric (e.g., `2025`)
- `most_recent_nhl_season_api_param()` returns `"20242025"` format
- PWHL: numeric year (e.g., `2024`)

## Testing

- **Framework:** testthat edition 3
- **Run all:** `devtools::test()`
- **Environment:** Set `NOT_CRAN=true` to run full test suite locally
- **Skip helpers:** `tests/testthat/helper-skip.R` provides `skip_nhl_test()`, `skip_phf_test()`, `skip_pwhl_test()` controlled by env vars `RUN_NHL_TESTS`, `RUN_PHF_TESTS`, `RUN_PWHL_TESTS` (NHL/PWHL default to `true`, PHF to `false`). CI workflow uses the same variable names.
- **Pattern:** `skip_on_cran()` + `skip_*_test()`, check `data.frame` + `fastRhockey_data` classes, validate `nrow(x) > 0`, check expected column names
- **xG tests:** additionally require `skip_if_not_installed("xgboost")`
- **Deprecated functions:** wrap calls in `suppressWarnings()` in tests
- **Reproducibility:** use specific game IDs/dates (e.g., `game_id = 2023020001`)

## Dependencies

**Imports:** cli, data.table, dplyr, glue, httr, janitor, jsonlite, lifecycle, lubridate, magrittr, purrr, Rcpp, RcppParallel, rlang, rvest, stringr, tibble, tidyr

**Suggests:** crayon, curl, DBI, furrr, future, ggplot2, ggrepel, progressr, qs, rmarkdown, RSQLite, stringi, stats, testthat (>= 3.0.0), usethis, xgboost, xml2
