<!-- DOCTOC SKIP -->

# GitHub Copilot Instructions

## Project Overview

fastRhockey is an R package for accessing hockey data from the **NHL**
(National Hockey League), the **PWHL** (Professional Women's Hockey League),
the AHL and the three Canadian major-junior leagues (OHL, WHL, QMJHL),
thirteen European leagues and ESPN men's/women's college hockey.
It wraps public web APIs (NHL API, HockeyTech, ESPN, Fox Sports and each
European league's site) to provide structured tibbles
of play-by-play, schedule, standings, roster, draft, player stats, and team
data. The package also ships with integrated **expected goals (xG)** models
trained via XGBoost and cached locally.

The package still exports the functions for the defunct **PHF** (Premier
Hockey Federation) league. They have been defunct since 1.0.0: each one
calls `lifecycle::deprecate_stop()` and stops with an error.

**Part of the [SportsDataverse](https://sportsdataverse.org/) family** of
R/Python packages for sports analytics.

- **Version:** 1.0.0 on CRAN; next release 1.1.0
- **R version:** >= 4.1.0
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
  helpers_nhl_edge.R        # .nhl_edge_api(), .nhl_edge_to_df() — NHL Edge URL builder
  helpers_nhl_records.R     # .nhl_records_api() — records.nhl.com URL builder
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
  nhl_meta.R                # Metadata + location + playoff-series metadata endpoints
  nhl_club_schedule.R       # Club-specific schedule
  nhl_club_stats_season.R   # Club-specific stats
  nhl_roster_season.R       # Roster by season
  nhl_conferences.R         # Conferences (derived from standings)
  nhl_conferences_info.R    # Conference details
  nhl_divisions.R           # Divisions (derived from standings)
  nhl_divisions_info.R      # Division details
  nhl_wsc_pbp.R             # WSC narrative-format play-by-play
  nhl_draft_tracker.R       # Live draft tracker
  nhl_ppt_replay.R          # Event-level video replay metadata
  nhl_ppt_replay_goal.R     # Goal-specific replay metadata
  nhl_postal_lookup.R       # Broadcast region lookup by postal code
  nhl_smartlinks.R          # NHL.com smart-link router

  # --- NHL Edge Analytics (35 functions: 33 nhl_edge_* + 2 nhl_cat_edge_*) ---
  # All wrappers share .nhl_edge_api() and .nhl_edge_to_df()
  nhl_edge_skater_*.R       # Detail, landing, comparison, shot location/speed,
                            # skating speed/distance, zone time, top-10 leaderboards
  nhl_edge_goalie_*.R       # Detail, landing, comparison, 5v5, save percentage,
                            # shot location, top-10 leaderboards
  nhl_edge_team_*.R         # Detail, landing, shot location/speed, skating
                            # speed/distance, zone time, top-10 leaderboards
  nhl_cat_edge_*.R          # CAT-framework variants of skater and goalie detail

  # --- NHL Records API (25 functions, records.nhl.com) ---
  # All wrappers share .nhl_records_api() with cayenneExp + pagination
  nhl_records_franchise*.R  # Franchise listing, detail, totals, team totals,
                            # season results, playoff appearances
  nhl_records_player*.R     # Player listing, by-team, stats
  nhl_records_skater_*.R    # Real-time stats by season and career
  nhl_records_goalie_*.R    # Career stats, season stats, shutout streak
  nhl_records_draft*.R      # Draft listing, lottery odds, lottery picks, prospects
  nhl_records_trophy.R      # Trophies
  nhl_records_award_details.R  # Award winners (filterable by seasonId)
  nhl_records_hof_players.R # Hall of Fame
  nhl_records_officials.R   # Officials
  nhl_records_attendance.R  # Attendance records
  nhl_records_venue.R       # Arena/venue listing
  nhl_records_combine.R     # Draft combine measurements

  # --- NHL Stats REST dedicated wrappers (19 nhl_stats_* functions) ---
  nhl_stats_franchise.R     # Franchise listing
  nhl_stats_players.R       # Players listing (requires cayenne_exp)
  nhl_stats_glossary.R      # Stat definition glossary
  nhl_stats_country.R       # Country lookup
  nhl_stats_config.R        # Config payload (non-tabular)
  nhl_stats_ping.R          # Health-check
  nhl_stats_skater_leaders.R   # Stats-API skater leaderboards by attribute
  nhl_stats_goalie_leaders.R   # Stats-API goalie leaderboards by attribute
  nhl_stats_skater_milestones.R   # Skater milestone achievements
  nhl_stats_goalie_milestones.R   # Goalie milestone achievements
  nhl_stats_team_listing.R  # Top-level team listing (and team/id/{id})
  nhl_stats_game_listing.R  # Top-level game listing
  nhl_stats_content_module.R   # NHL.com CMS content modules

  # --- NHL helper aggregators ---
  nhl_game_ids_by_season.R     # All game IDs across teams for a season
  nhl_all_players_by_season.R  # All rostered players across teams for a season
  nhl_player_career_stats.R    # Player landing seasonTotals -> career frame
  nhl_team_summary_range.R     # Multi-season team summary
  nhl_skater_summary_range.R   # Multi-season skater summary
  nhl_goalie_summary_range.R   # Multi-season goalie summary

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
  pwhl_loaders.R            # load_pwhl_pbp, _player_box, _skater_box, _goalie_box,
                            # _team_box, _schedule, _rosters, _game_rosters,
                            # _game_info, _scoring_summary, _penalty_summary,
                            # _three_stars, _officials, _shots_by_period,
                            # _shootout, _shifts, _xg_pbp, update_pwhl_db
                            # All share .pwhl_release_loader() (catalog row +
                            # thin wrapper to add a new dataset)

  # --- Data Loaders (from sportsdataverse data repos) ---
  nhl_loaders.R             # load_nhl_pbp, _pbp_lite, _player_box, _skater_box, _goalie_box,
                            # _team_box, _schedule, _rosters, _game_rosters, _game_info,
                            # _scoring, _penalties, _three_stars, _scratches, _linescore,
                            # _shifts, _officials, _shots_by_period, _shootout,
                            # update_nhl_db (+ build_nhl_db, get_missing_nhl_games)
                            # The 19 season loaders (+ 8 aliases) share
                            # .nhl_release_loader() (RDS assets, matching PWHL)
  nhl_group_loaders.R       # load_nhl_groups, _group_seasons, _group_aliases,
                            # _team_group_seasons: .groups_release_loader()
                            # reads CSV assets from the nhl_groups release
                            # nhl_schedule() also exposes include_data_flags = FALSE to
                            # left-join the cached games-in-data-repo flags onto live
                            # schedules.

  # --- ESPN, Fox and HockeyTech CHL/AHL ---
  espn_nhl_*.R              # 126 espn_nhl_* wrappers (site / web / core APIs)
  fox_nhl.R                 # 8 fox_nhl_* wrappers (Fox Sports Bifrost)
  ahl.R, ohl.R, whl.R, qmjhl.R # ahl_* / ohl_* / whl_* / qmjhl_* (12 each)
  hockeytech_*.R            # 5-league HockeyTech registry and shared helpers

  # --- European leagues (211 functions) ---
  euro_hockey_runtime.R     # .polite_perform(), .euro_get(): 1 s per-host
                            # spacing, 429/5xx retry, classed conditions
  esports.R, esports_leagues.R # del_*, icehl_*, eihl_*, metal_*
  khl.R, vhl.R, shl.R, ha.R, liiga.R, sihf.R, nationalleague.R (nl_*),
  elh.R, magnus.R           # one file per league

  # --- ESPN college hockey (242 functions) ---
  espn_ncaa_hockey.R        # espn_mch_* and espn_wch_* (121 each)

tests/testthat/             # Unit tests (testthat edition 3)
man/                        # Auto-generated roxygen2 documentation
data/                       # Included datasets (.rda)
data-raw/                   # Scripts for generating package data (*.md untracked)
vignettes/                  # Vignettes for pkgdown site
```

### API Architecture

The NHL and PWHL backends (ESPN, Fox Sports, the other HockeyTech leagues and
the European league APIs are covered in the file map above):

| Backend | Base URL | Used By | Auth |
|---------|----------|---------|------|
| NHL Web API | `api-web.nhle.com/v1/` | Game feed, schedule, standings, rosters, gamecenter, draft, scoreboard, scores, meta, location, partner-game, where-to-watch, smartlinks, postal-lookup, ppt-replay, wsc | None |
| NHL Edge API | `api-web.nhle.com/v1/edge/...` and `v1/cat/edge/...` | Skater/goalie/team Edge advanced metrics (shot location, shot speed, skating speed, skating distance, zone time, comparisons, top-10 leaderboards) | None |
| NHL Stats API | `api.nhle.com/stats/rest/{lang}/` | Skater/goalie/team stats, draft stats, seasons, franchise, players, glossary, country, config, leaders, milestones | None |
| NHL Records API | `records.nhl.com/site/api/` | Franchise totals, player/skater/goalie career and real-time stats, draft lottery, hall of fame, trophies, awards, attendance, venues, officials, combine | None |
| HockeyTech (statviewfeed) | `lscluster.hockeytech.com/feed/?feed=statviewfeed` | PWHL schedule, standings, roster, PBP, box scores, stats | Public key in URL |
| HockeyTech (modulekit) | `lscluster.hockeytech.com/feed/?feed=modulekit` | PWHL seasons, player info, leaders, transactions, streaks, brackets, scorebar | Public key in URL |
| HockeyTech (gc) | `lscluster.hockeytech.com/feed/?feed=gc` | PWHL game summary | Public key in URL |

- **NHL Web API** returns clean JSON; parsed with `jsonlite::fromJSON()`
- **NHL Edge API** uses an internal `.nhl_edge_api(base, season, game_type, prefix)` helper that handles the `/now` vs `/{season}/{gameType}` URL split, plus `.nhl_edge_to_df()` to normalize response shapes
- **NHL Stats API** uses Cayenne filter expressions for query params (proprietary). Note: the `leaders/{skaters,goalies}/{attribute}` endpoint does **not** accept `start`/`limit` query parameters (returns 500); valid goalie attributes are restricted to `savePctg`, `gaa`, and `shutouts`
- **NHL Records API** uses an internal `.nhl_records_api(resource, cayenne_exp, sort, limit, start, query)` helper. The records API does **not** support path-suffix filtering (`franchise/{id}` returns 404); use `cayenneExp` filtering instead
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
- **PHF functions** (21 functions: 14 `phf_*`, 5 `load_phf_*`, `update_phf_db()`, `most_recent_phf_season()`): defunct since v1.0.0 — league ceased operations
- All deprecated functions emit `lifecycle::deprecate_stop()` and raise errors
- Tests for deprecated functions check for `lifecycle_error_deprecated` class

## Key Conventions

### Coding Style

- **One exported function per file** (filename matches function name)
- **roxygen2** for all documentation; regenerate with `devtools::document()`
- **Pipe-first style**: functions use `%>%` (magrittr) extensively
- **janitor::clean_names()** applied to all API responses for consistent snake_case columns
- **httr2 helpers** for all HTTP requests (`httr` is not a dependency): `.retry_request(url, params = ...)` in `R/utils.R` (`httr2::req_retry()`, proxy via `getOption("fastRhockey.proxy")`), then `check_status()` and `.resp_text()` / `.resp_json()`
- **Three failure contracts.** NHL and PWHL wrappers use `tryCatch` with `message()` and return `NULL`. ESPN (NHL and college), Fox and AHL/OHL/WHL/QMJHL wrappers report through `.report_api_error()` / `cli` and return an empty data frame. European wrappers perform through `.polite_perform()` / `.euro_get()` (`R/euro_hockey_runtime.R`; only 429/500/502/503/504 and connection failures are retried) and raise classed conditions, all inheriting `fastRhockey_error`: `fastRhockey_no_data` (404 / nothing there), `fastRhockey_bad_request` (400 / 422) and `fastRhockey_asset_fetch` (any other failure, including an exhausted 429)
- **glue::glue()** for string interpolation and URL construction
- Internal helper functions prefixed with `.` (e.g., `.parse_game_rosters()`, `.build_pbp()`)
- **HockeyTech JSONP**: strip the callback envelope ONLY via `.hockeytech_api()` (`R/hockeytech_helpers.R`) — never a per-function regex on the callback tail; the tail shape varies by payload and hardcoded `gsub` fixes have silently emptied results
- **Live tests always skip on CRAN and CI; run them locally** (gate them with the family's `skip_<family>_test()`, which calls `skip_on_cran()` and `skip_on_ci()`); offline tests (mocks, fixtures, captured bodies) run everywhere
- **Release loaders**: one loader per sportsdataverse-data hockey tag; add a dataset = one `(release_tag, file_prefix)` catalog row + a thin wrapper on `.nhl_release_loader()` / `.pwhl_release_loader()` (the conference/division tables use `.groups_release_loader()` in `R/nhl_group_loaders.R`)
- `globalVariables()` declarations in `utils.R` suppress R CMD check NSE notes

### Season Format

- **NHL Web API:** `"20242025"` — concatenated year strings
- **NHL Stats API Cayenne filters:** `"20242025"` — same format
- **most_recent_nhl_season():** Returns end year as numeric (e.g., `2025` for 2024-25 season)
- **most_recent_nhl_season_api_param():** Returns `"20242025"` format
- **PWHL, HockeyTech, ESPN and the European leagues:** numeric END year (e.g., `2026` = 2025-26)
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
        res <- .retry_request(url)
        check_status(res)
        raw <- jsonlite::fromJSON(.resp_text(res), flatten = TRUE)
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
- **Environment:** live tests always skip on CRAN and CI (the workflows pass `RUN_*_TESTS` through from repository variables, but `skip_on_ci()` wins); run them locally with `NOT_CRAN=true` and the family toggle set to `1`
- **Skip helpers:** `tests/testthat/helper-skip.R` provides `skip_nhl_test()`, `skip_pwhl_test()`, `skip_espn_test()`, `skip_euro_test()` and `skip_phf_test()`, all through `skip_live_test()`, controlled by `RUN_NHL_TESTS`, `RUN_PWHL_TESTS`, `RUN_ESPN_TESTS` (ESPN NHL), `RUN_EURO_TESTS` (European leagues and ESPN college hockey) and `RUN_PHF_TESTS`. A family runs only when its toggle is `1` or `true`; unset means skip. `skip_slow_nhl_test()` (`RUN_SLOW_NHL_TESTS`, on top of `RUN_NHL_TESTS`) gates the two slow season-wide NHL aggregator tests

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
- `skip_on_cran()` plus the family's skip helper on all tests that hit live APIs
- Test both `data.frame` and `fastRhockey_data` S3 classes
- Validate row counts > 0
- Check for expected column names
- Use specific game IDs for reproducible tests (e.g., `2023020001`)
- xG tests additionally require `skip_if_not_installed("xgboost")`
- Deprecated function tests: wrap calls in `suppressWarnings()`

## Dependencies

### Imports (required)

cli, data.table, dplyr, glue, httr2, janitor, jsonlite, lifecycle, lubridate,
magrittr, purrr, rlang, rvest, stats, stringr, tibble, tidyr, utils, xml2

### Suggests (optional)

crayon, curl, DBI, progressr, RSQLite, testthat (>= 3.1.7), withr, xgboost

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
- `nhl-edge` — NHL Edge analytics family (`nhl_edge_*`, `nhl_cat_edge_*`)
- `nhl-records` — NHL Records API family (`nhl_records_*`)
- `nhl-stats` — NHL Stats REST family (`nhl_stats_*`)
- `espn`, `fox`, `hockeytech` — ESPN (NHL and college), Fox Sports, and AHL/OHL/WHL/QMJHL families
- `euro` (or the league prefix, e.g. `khl`, `shl`) — European league families
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

## Cheat sheet

There is a printable one-page reference for this package at
<https://sportsdataverse.org/cheatsheets/fastRhockey.pdf>, one of [a set covering every SportsDataverse package](https://sportsdataverse.org/cheatsheets).
Keep it in mind when adding or renaming an exported function: the sheet is a
hand-built canvas, so a surface change means the sheet needs a revision too.
