## Test environments

* local Windows 10 Pro (10.0.19045), R 4.4.x
* GitHub Actions: ubuntu-latest (release), windows-latest (release),
  macos-latest (release), ubuntu-latest (oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Changes in this version

### New PWHL Functions

* Player endpoints: `pwhl_player_info()`, `pwhl_player_game_log()`,
  `pwhl_player_stats()`, `pwhl_player_search()`
* League endpoints: `pwhl_leaders()`, `pwhl_transactions()`,
  `pwhl_streaks()`, `pwhl_playoff_bracket()`
* Game endpoints: `pwhl_game_summary()`, `pwhl_scorebar()`
* Data loaders: `load_pwhl_pbp()`, `load_pwhl_player_box()`,
  `load_pwhl_schedule()`, `load_pwhl_rosters()`, `update_pwhl_db()`
* Utilities: `most_recent_pwhl_season()`

### Improvements

* `pwhl_season_id()` now queries the HockeyTech API dynamically
  (with hardcoded fallback)
* Removed `globalVariables()` (~90 entries) in favour of proper `.data$`
  pronoun masking and string-based tidy selection throughout
* Replaced deprecated `dplyr::mutate_at()` with `dplyr::across()`
* Silenced `packageStartupMessage()` in `.onLoad()` per CRAN policy
* Fixed `\itemize` Rd syntax issues in `update_nhl_db` and `update_phf_db`

## Downstream dependencies

No downstream dependencies on CRAN.
