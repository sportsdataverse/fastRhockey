# PR Development Notes (v1.0.0 - PWHL Expansion + Code Quality)

86 files changed, +7 574, −4 860

## Summary

Expand PWHL coverage to all documented HockeyTech API endpoints per the
[PWHL-Data-Reference](https://github.com/sportsdataverse/PWHL-Data-Reference).
Remove `globalVariables()` in favour of proper `.data$` masking and string-based
tidy selection. Fix pre-existing R CMD check notes. Result: **0 errors, 0
warnings, 0 notes**.

## Commits

```
f7eb5d7 docs(pwhl): expand return docs, update package metadata and README
658c86c fix: resolve pre-existing R CMD check notes
e0fd972 refactor: replace globalVariables with .data$ masking
0c39912 feat(pwhl): add data loaders and database builder
0a24210 feat(pwhl): add game summary and scorebar endpoints
d44e20b feat(pwhl): add leaders, transactions, streaks, and playoff bracket
01cbbdc feat(pwhl): add player info, game log, season stats, and search
b47c890 feat(pwhl): add internal helpers and dynamic season lookup
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
