# NA

## Test environments

- local Windows 10 Pro (10.0.19045), R 4.4.x
- GitHub Actions: ubuntu-latest (release), windows-latest (release),
  macos-latest (release), ubuntu-latest (oldrel-1)

## R CMD check results

0 errors \| 0 warnings \| 0 notes

## Changes in this version

### New PWHL Functions

- Player endpoints:
  [`pwhl_player_info()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_info.md),
  [`pwhl_player_game_log()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_game_log.md),
  [`pwhl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_stats.md),
  [`pwhl_player_search()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_search.md)
- League endpoints:
  [`pwhl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_leaders.md),
  [`pwhl_transactions()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_transactions.md),
  [`pwhl_streaks()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_streaks.md),
  [`pwhl_playoff_bracket()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_playoff_bracket.md)
- Game endpoints:
  [`pwhl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_game_summary.md),
  [`pwhl_scorebar()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_scorebar.md)
- Data loaders:
  [`load_pwhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_pbp.md),
  [`load_pwhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_player_box.md),
  [`load_pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_schedule.md),
  [`load_pwhl_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_rosters.md),
  [`update_pwhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_pwhl_db.md)
- Utilities:
  [`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md)

### Improvements

- [`pwhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_season_id.md)
  now queries the HockeyTech API dynamically (with hardcoded fallback)
- Removed
  [`globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
  (~90 entries) in favour of proper `.data$` pronoun masking and
  string-based tidy selection throughout
- Replaced deprecated
  [`dplyr::mutate_at()`](https://dplyr.tidyverse.org/reference/mutate_all.html)
  with
  [`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html)
- Silenced
  [`packageStartupMessage()`](https://rdrr.io/r/base/message.html) in
  `.onLoad()` per CRAN policy
- Fixed `\itemize` Rd syntax issues in `update_nhl_db` and
  `update_phf_db`

## Downstream dependencies

No downstream dependencies on CRAN.
