<!-- DOCTOC SKIP -->

# GitHub Copilot Instructions

## Project Overview

fastRhockey is an R package for accessing hockey data from the **NHL**
(National Hockey League) and **PWHL** (Professional Women's Hockey League).
It wraps public web APIs (NHL API, HockeyTech) to provide structured tibbles
of play-by-play, schedule, standings, roster, draft, player stats, and team
data. The package also ships with integrated **expected goals (xG)** models
trained via XGBoost and cached locally.

The package includes deprecated functions for the defunct **PHF** (Premier
Hockey Federation) league. These emit lifecycle warnings and will be removed
in a future release.

**Part of the [SportsDataverse](https://sportsdataverse.org/) family** of
R/Python packages for sports analytics.

- **Version:** 1.0.0
- **R version:** >= 4.0.0
- **License:** MIT
- **Documentation site:** <https://fastRhockey.sportsdataverse.org/>
- **GitHub:** <https://github.com/sportsdataverse/fastRhockey>

## Build & Development Commands

```bash
# Install all dependencies (dev + suggests)
install.packages("devtools")
devtools::install_deps(dependencies = TRUE)

# Run full R CMD check
devtools::check()

# Build documentation (roxygen2)
devtools::document()

# Run tests
devtools::test()
# or
testthat::test_local()

# Run a single test file
testthat::test_file("tests/testthat/test-nhl_standings.R")

# Build and install locally
devtools::install()

# Build pkgdown site
pkgdown::build_site()
```

## Architecture

### Package Structure

```
R/                          # Source code (one function per file, mostly)
  utils.R                   # Shared helpers: check_status, most_recent_*_season, messaging
  zzz.R                     # .onLoad() — xG model download/caching lifecycle
  fastRhockey-package.R     # Package-level roxygen doc, lifecycle import
  helpers_nhl.R             # helper_nhl_calculate_xg(), helper_nhl_prepare_xg_data()
  helpers_phf.R             # PHF helper functions (deprecated)
  data.R                    # Roxygen docs for included datasets
  espn_nhl_teams.R          # ESPN team reference data

  # --- NHL Functions (api-web.nhle.com + api.nhle.com/stats) ---
  nhl_game_feed.R           # Game feed with PBP, rosters, game info + nhl_game_pbp()
  nhl_game_boxscore.R       # Game boxscore (skater/goalie stats)
  nhl_game_shifts.R         # Shift data
  nhl_game_story.R          # Game story/narrative
  nhl_game_content.R        # Game content (media)
  nhl_gamecenter.R          # Gamecenter landing & right rail
  nhl_schedule.R            # Schedule (by date or full season)
  nhl_schedule_calendar.R   # Calendar-formatted schedules
  nhl_standings.R           # Current/historical standings
  nhl_standings_season.R    # Standings season list
  nhl_teams.R               # Team info
  nhl_teams_roster.R        # Team rosters
  nhl_teams_stats.R         # Team per-player stats
  nhl_teams_info.R          # Team info by abbreviation
  nhl_team_prospects.R      # Team prospects
  nhl_team_scoreboard.R     # Team-specific scoreboard
  nhl_player_info.R         # Player biographical info
  nhl_player_game_log.R     # Player game logs
  nhl_player_stats.R        # Player season stats
  nhl_player_spotlight.R    # Featured player spotlight
  nhl_stats_goalies.R       # Goalie stats (api.nhle.com/stats)
  nhl_stats_skaters.R       # Skater stats (api.nhle.com/stats)
  nhl_stats_teams.R         # Team stats (api.nhle.com/stats)
  nhl_stats_misc.R          # Miscellaneous stats + draft + stats seasons
  nhl_goalie_stats_leaders.R# Goalie leader rankings
  nhl_skater_stats_leaders.R# Skater leader rankings
  nhl_draft.R               # Draft data
  nhl_draft_year.R          # Draft by year
  nhl_draft_prospects.R     # Draft prospects
  nhl_draft_prospects_info.R# Draft prospect details
  nhl_seasons.R             # Available seasons + draft rankings
  nhl_scoreboard.R          # Daily scoreboard
  nhl_scores.R              # Scores
  nhl_playoffs.R            # Playoff brackets, schedules, carousels
  nhl_tv_schedule.R         # TV broadcast schedule
  nhl_where_to_watch.R      # Where-to-watch info
  nhl_meta.R                # Metadata + location endpoints
  nhl_club_schedule.R       # Club-specific schedule
  nhl_club_stats_season.R   # Club-specific stats
  nhl_roster_season.R       # Roster by season
  nhl_conferences.R         # Conferences (derived from standings)
  nhl_conferences_info.R    # Conference details
  nhl_divisions.R           # Divisions (derived from standings)
  nhl_divisions_info.R      # Division details

  # --- Deprecated PHF Functions ---
  phf_game.R                # PHF game data (all, details, raw, summary)
  phf_game_pbp.R            # PHF play-by-play (deprecated)
  phf_pbp.R                 # PHF loaders + update_phf_db
  phf_schedule.R            # PHF schedule
  phf_standings.R           # PHF standings
  phf_player_box.R          # PHF player box scores
  phf_player_stats.R        # PHF player stats
  phf_team_box.R            # PHF team box scores
  phf_team_roster.R         # PHF team rosters
  phf_team_stats.R          # PHF team stats
  phf_leaders.R             # PHF league leaders
  phf_league_info.R         # PHF league info

  # --- PWHL Functions (lscluster.hockeytech.com) ---
  pwhl_helpers.R            # .pwhl_api(), .pwhl_modulekit_url(), .pwhl_gc_url()
  pwhl_schedule.R           # PWHL schedule
  pwhl_standings.R          # PWHL standings
  pwhl_pbp.R                # PWHL play-by-play
  pwhl_player_box.R         # PWHL player box scores
  pwhl_game_info.R          # PWHL game info
  pwhl_game_summary.R       # PWHL game summary (gc feed)
  pwhl_teams.R              # PWHL team list
  pwhl_team_roster.R        # PWHL team rosters
  pwhl_stat_leaders.R       # PWHL stat leaders
  pwhl_season_id.R          # PWHL season ID lookup (dynamic API + fallback)
  pwhl_player_info.R        # PWHL player profile
  pwhl_player_game_log.R    # PWHL player game-by-game stats
  pwhl_player_stats.R       # PWHL player career/season stats
  pwhl_player_search.R      # PWHL player search
  pwhl_leaders.R            # PWHL league leaders (top scorers/goalies)
  pwhl_transactions.R       # PWHL player transactions
  pwhl_streaks.R            # PWHL player streaks
  pwhl_playoff_bracket.R    # PWHL playoff bracket
  pwhl_scorebar.R           # PWHL scorebar (recent/upcoming scores)
  pwhl_loaders.R            # load_pwhl_pbp, _player_box, _schedule, _rosters, update_pwhl_db

  # --- Data Loaders (from sportsdataverse data repos) ---
  nhl_pbp.R                 # load_nhl_pbp, _player_box, _rosters, _schedule, _team_box, update_nhl_db

tests/testthat/             # Unit tests (testthat edition 3)
man/                        # Auto-generated roxygen2 documentation
data/                       # Included datasets (.rda)
data-raw/                   # Scripts for generating package data (*.md untracked)
vignettes/                  # Vignettes for pkgdown site
```

### API Architecture

Three distinct backends:

| Backend | Base URL | Used By | Auth |
|---------|----------|---------|------|
| NHL Web API | `api-web.nhle.com/v1/` | Game feed, schedule, standings, rosters, gamecenter | None |
| NHL Stats API | `api.nhle.com/stats/rest/{lang}/` | Skater/goalie/team stats, draft stats, seasons | None |
| HockeyTech (statviewfeed) | `lscluster.hockeytech.com/feed/?feed=statviewfeed` | PWHL schedule, standings, roster, PBP, box scores, stats | Public key in URL |
| HockeyTech (modulekit) | `lscluster.hockeytech.com/feed/?feed=modulekit` | PWHL seasons, player info, leaders, transactions, streaks, brackets, scorebar | Public key in URL |
| HockeyTech (gc) | `lscluster.hockeytech.com/feed/?feed=gc` | PWHL game summary | Public key in URL |

- **NHL Web API** returns clean JSON; parsed with `jsonlite::fromJSON()`
- **NHL Stats API** uses Cayenne filter expressions for query params (proprietary)
- **HockeyTech** returns JSONP with Angular callbacks that must be regex-stripped before parsing

### S3 Class: `fastRhockey_data`

All data-returning functions wrap their output in the `fastRhockey_data` S3
class via `make_fastRhockey_data()`. This adds metadata attributes (source
description, timestamp) and provides a custom `print()` method.

### xG Model Pipeline

1. **On package load** (`.onLoad` in `zzz.R`): Downloads XGBoost JSON models
   and feature-name metadata from
   `sportsdataverse/fastRhockey-nhl-data/main/models/`
2. **Cached** in `tools::R_user_dir("fastRhockey", "cache")` — only downloaded once
3. **Three models:** 5v5, special teams, penalty shots
4. **Models stored** in `.xg_env` (a package environment in `zzz.R`), accessed
   via `.xg_env$xg_model_5v5` etc. in `helpers_nhl.R`
5. **helper_nhl_calculate_xg()** runs predictions; adds an `xg` column only
   for shot-type events
6. **Requires** the `xgboost` suggested package; fails gracefully if absent

### Deprecation Strategy

The package uses `lifecycle` for formal deprecation of PHF functions:
- **PHF functions** (21 functions): Deprecated in v1.0.0 — league ceased operations
- All deprecated functions emit `lifecycle::deprecate_stop()` and raise errors
- Tests for deprecated functions check for `lifecycle_error_deprecated` class

## Key Conventions

### Coding Style

- **One exported function per file** (filename matches function name)
- **roxygen2** for all documentation; regenerate with `devtools::document()`
- **Pipe-first style**: functions use `%>%` (magrittr) extensively
- **janitor::clean_names()** applied to all API responses for consistent snake_case columns
- **httr::RETRY("GET", ...)** for all HTTP requests (built-in retry on transient failures)
- **tryCatch** with `message()` for error handling — functions return `NULL` on failure, not errors
- **glue::glue()** for string interpolation and URL construction
- Internal helper functions prefixed with `.` (e.g., `.parse_game_rosters()`, `.build_pbp()`)
- `globalVariables()` declarations in `utils.R` suppress R CMD check NSE notes

### Season Format

- **NHL Web API:** `"20242025"` — concatenated year strings
- **NHL Stats API Cayenne filters:** `"20242025"` — same format
- **most_recent_nhl_season():** Returns end year as numeric (e.g., `2025` for 2024-25 season)
- **most_recent_nhl_season_api_param():** Returns `"20242025"` format
- **PWHL:** Numeric year (e.g., `2025`)
- **most_recent_pwhl_season():** Returns concluding year as numeric (e.g., `2025`)
- **pwhl_season_id():** Maps (season, game_type) to HockeyTech season_id dynamically

### Naming Conventions

- **Exported functions:** `league_entity_action()` pattern (e.g., `nhl_teams_roster()`, `pwhl_schedule()`)
- **Internal functions:** `.snake_case()` with leading dot
- **Test files:** `test-function_name.R` (matches source filename without `.R`)
- **Data files:** snake_case `.rda` files in `data/`
- **Parameters:** `snake_case` (e.g., `game_id`, `team_abbr`, `season`)

### Error Handling Pattern

```r
tryCatch(
    expr = {
        res <- httr::RETRY("GET", url)
        check_status(res)
        # ... parse and return
    },
    error = function(e) {
        message(glue::glue("{Sys.time()}: Error in function_name: {e$message}"))
        return(NULL)
    }
)
```

## Testing

- **Framework:** testthat edition 3
- **Location:** `tests/testthat/`
- **Run all:** `devtools::test()` or `testthat::test_local()`
- **Test naming:** `test-<function_name>.R`
- **Environment:** Set `NOT_CRAN=true` for full test suite on CI
- **Skip helpers:** `tests/testthat/helper-skip.R` provides `skip_nhl_test()`, `skip_phf_test()`, `skip_pwhl_test()` controlled by env vars `RUN_NHL_TESTS`, `RUN_PHF_TESTS`, `RUN_PWHL_TESTS` (NHL/PWHL default `true`, PHF default `false`)

### Test Pattern

```r
test_that("NHL - Descriptive test name", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_function(param = value)
    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
    expect_true("expected_column" %in% names(x))
})
```

**Key conventions:**
- `skip_on_cran()` on all tests that hit live APIs
- Test both `data.frame` and `fastRhockey_data` S3 classes
- Validate row counts > 0
- Check for expected column names
- Use specific game IDs for reproducible tests (e.g., `2023020001`)
- xG tests additionally require `skip_if_not_installed("xgboost")`
- Deprecated function tests: wrap calls in `suppressWarnings()`

## Dependencies

### Imports (required)

cli, data.table, dplyr, glue, httr, janitor, jsonlite, lifecycle, lubridate,
magrittr, purrr, Rcpp, RcppParallel, rlang, rvest, stringr, tibble, tidyr

### Suggests (optional)

crayon, curl, DBI, furrr, future, ggplot2, ggrepel, progressr, qs, rmarkdown,
RSQLite, stringi, stats, testthat (>= 3.0.0), usethis, xgboost, xml2

## Commit Message Guidelines

Use Conventional Commits format:

```text
<type>(<scope>): <description>
```

### Types

- **feat**: New feature or function
- **fix**: Bug fix
- **docs**: Documentation only changes
- **style**: Formatting, whitespace (no logic change)
- **refactor**: Code change that neither fixes a bug nor adds a feature
- **test**: Adding or correcting tests
- **build**: Build system or dependency changes
- **ci**: CI configuration changes
- **chore**: Other changes that don't modify R/ or tests/

### Scopes

- `nhl`, `phf`, `pwhl` — league-specific changes
- `xg` — xG model pipeline
- `pkgdown` — documentation site
- `ci` — GitHub Actions workflows
- `loader` — data loader functions

## Commit Authorship

- **Never** include AI tools (Claude, Copilot, ChatGPT, etc.) as a co-author or author in commit messages. Commits should be attributed solely to the human contributors who made or directed the changes.

## Security & Data Privacy

- **No authentication tokens** should be committed — all APIs are public
- The HockeyTech API key is embedded in the source (public key shared across
  HockeyTech-consuming packages) — this is intentional and expected
- **Never** commit user cache paths or local file paths
- xG model files are downloaded from a public GitHub repository — no credentials needed
