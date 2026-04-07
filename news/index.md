# Changelog

## **fastRhockey 1.0.0**

#### **Breaking Changes**

- **Version 1.0.0** signals API stability for NHL and PWHL endpoints.
- All PHF functions are formally deprecated via `lifecycle`. The Premier
  Hockey Federation ceased operations; use PWHL functions instead.
  Functions will be removed in a future release.
- Consolidated new NHL API functions into existing NHL function names
  rather than creating `_v2` variants, since the original API endpoints
  were deprecated by the NHL. This means
  [`nhl_game_feed()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_feed.md),
  [`nhl_game_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_boxscore.md),
  [`nhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_schedule.md),
  [`nhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams.md),
  [`nhl_teams_roster()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams_roster.md),
  and
  [`nhl_player_info()`](https://fastRhockey.sportsdataverse.org/reference/nhl_player_info.md)
  now use the new `api-web.nhle.com` endpoints directly.

#### **New PWHL Functions**

- [`pwhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_pbp.md)
  – PWHL play-by-play data.
- [`pwhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_box.md)
  – PWHL player box scores (skaters and goalies).
- [`pwhl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_game_info.md)
  – PWHL game information and metadata.
- [`pwhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_standings.md)
  – PWHL league standings.
- [`pwhl_stats()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_stats.md)
  – PWHL stat leaders (skaters and goalies).
- [`pwhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_season_id.md)
  – PWHL season ID lookup.

#### **New NHL Functions**

- [`nhl_where_to_watch()`](https://fastRhockey.sportsdataverse.org/reference/nhl_where_to_watch.md)
  – streaming/broadcast availability.
- [`nhl_partner_game_odds()`](https://fastRhockey.sportsdataverse.org/reference/nhl_partner_game_odds.md)
  – partner game odds by country.
- NHL API migration to `api-web.nhle.com`:
  [`nhl_game_feed()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_feed.md),
  [`nhl_game_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_boxscore.md),
  [`nhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_schedule.md),
  [`nhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams.md),
  [`nhl_teams_roster()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams_roster.md),
  [`nhl_player_info()`](https://fastRhockey.sportsdataverse.org/reference/nhl_player_info.md),
  [`nhl_player_game_log()`](https://fastRhockey.sportsdataverse.org/reference/nhl_player_game_log.md),
  [`nhl_stats_goalies()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_goalies.md),
  [`nhl_stats_skaters()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_skaters.md),
  [`nhl_stats_teams()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_teams.md),
  [`nhl_stats_misc()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_misc.md),
  [`nhl_gamecenter_landing()`](https://fastRhockey.sportsdataverse.org/reference/nhl_gamecenter_landing.md),
  [`nhl_gamecenter_right_rail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_gamecenter_right_rail.md),
  [`nhl_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/nhl_scoreboard.md),
  [`nhl_scores()`](https://fastRhockey.sportsdataverse.org/reference/nhl_scores.md),
  [`nhl_seasons()`](https://fastRhockey.sportsdataverse.org/reference/nhl_seasons.md),
  [`nhl_tv_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_tv_schedule.md),
  [`nhl_game_story()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_story.md),
  [`nhl_game_content()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_content.md),
  [`nhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_shifts.md),
  [`nhl_playoff_bracket()`](https://fastRhockey.sportsdataverse.org/reference/nhl_playoff_bracket.md),
  [`nhl_playoff_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_playoff_schedule.md),
  [`nhl_playoff_carousel()`](https://fastRhockey.sportsdataverse.org/reference/nhl_playoff_carousel.md).
- xG model integration via
  [`helper_nhl_calculate_xg()`](https://fastRhockey.sportsdataverse.org/reference/helper_nhl_calculate_xg.md).

#### **Bug Fixes**

- Fixed
  [`nhl_draft_year()`](https://fastRhockey.sportsdataverse.org/reference/nhl_draft_year.md)
  – NHL API removed the `/v1/draft/picks/{year}` endpoint. The function
  now iterates over rounds 1-7 using `/v1/draft/picks/{year}/{round}`.
- Fixed
  [`pwhl_stats()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_stats.md)
  – resolved “object ‘players’ not found” error when API calls failed.
  Fixed team ID resolution for skater stats.
- Fixed
  [`pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_schedule.md)
  – the `season` column was inadvertently dropped from the output. It is
  now included.
- Fixed
  [`refresh_xg_models()`](https://fastRhockey.sportsdataverse.org/reference/refresh_xg_models.md)
  – resolved “cannot change value of locked binding” error by storing xG
  models in a package environment (`.xg_env`) instead of top-level
  bindings.
- Fixed NAMESPACE: removed `import(tidyverse)` which violated CRAN
  policy. Individual packages (dplyr, tidyr, etc.) are already imported.

#### **Deprecations**

- PHF functions:
  [`phf_schedule()`](https://fastRhockey.sportsdataverse.org/reference/phf_schedule.md),
  [`phf_standings()`](https://fastRhockey.sportsdataverse.org/reference/phf_standings.md),
  [`phf_pbp()`](https://fastRhockey.sportsdataverse.org/reference/phf_pbp.md),
  [`phf_player_box()`](https://fastRhockey.sportsdataverse.org/reference/phf_player_box.md),
  [`phf_team_box()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_box.md),
  [`phf_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_roster.md),
  [`phf_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_stats.md),
  [`phf_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/phf_player_stats.md),
  [`phf_leaders()`](https://fastRhockey.sportsdataverse.org/reference/phf_leaders.md),
  [`phf_league_info()`](https://fastRhockey.sportsdataverse.org/reference/phf_league_info.md),
  [`phf_game_all()`](https://fastRhockey.sportsdataverse.org/reference/phf_game_all.md),
  [`phf_game_raw()`](https://fastRhockey.sportsdataverse.org/reference/phf_game_raw.md),
  [`phf_game_details()`](https://fastRhockey.sportsdataverse.org/reference/phf_game_details.md),
  [`phf_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/phf_game_summary.md).
- PHF loaders:
  [`load_phf_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_pbp.md),
  [`load_phf_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_team_box.md),
  [`load_phf_player_box()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_player_box.md),
  [`load_phf_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_schedule.md),
  [`load_phf_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_rosters.md),
  [`update_phf_db()`](https://fastRhockey.sportsdataverse.org/reference/update_phf_db.md).
- Utility:
  [`most_recent_phf_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_phf_season.md).

#### **Improvements**

- Added `lifecycle` package for formal deprecation management.
- Updated `testthat` dependency to `>= 3.0.0`.
- Complete test coverage for all 95 exported functions (482 tests).
- Added environment-controlled test toggles (`RUN_NHL_TESTS`,
  `RUN_PHF_TESTS`, `RUN_PWHL_TESTS`) via `tests/testthat/helper-skip.R`.
- CI workflow environment variables now match `helper-skip.R` names.
- Updated test expectations to match current API responses.
- [`nhl_where_to_watch()`](https://fastRhockey.sportsdataverse.org/reference/nhl_where_to_watch.md)
  returns `NULL` gracefully when the NHL `/v1/where-to-watch` endpoint
  is unavailable.
- Updated `_pkgdown.yml` with reorganized reference sections and
  deprecated function categories.

#### **Documentation & Infrastructure**

- Updated `CONTRIBUTING.md` with naming conventions, testing environment
  variables, conventional commits guide, and deprecation process.
- Updated PR and issue templates.
- Updated `CLAUDE.md` and `.github/copilot-instructions.md` to reflect
  v1.0.0 changes.
- Added `data-raw/PR_devel.md` and `data-raw/NEWS_devel.md` development
  scratchpads (not tracked by git).

## **fastRhockey 0.7.0**

#### **PWHL functions added**

- [`pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_schedule.md)
  function added.
- [`pwhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_team_roster.md)
  function added.
- [`pwhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_teams.md)
  function added.

## **fastRhockey 0.6.0**

- Improved resiliency for several PHF functions, updates under the hood.

## **fastRhockey 0.5.0**

- Major improvements to NHL Game PBP Data parsing with shifts in-line
  via
  [`nhl_game_pbp()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_pbp.md)
  function added to match [`hockeyR`](https://hockeyr.netlify.app).

## **fastRhockey 0.4.2**

- Fixing issues with
  [`phf_league_info()`](https://fastRhockey.sportsdataverse.org/reference/phf_league_info.md)
  function for team name inconsistency.

## **fastRhockey 0.4.1**

- Minor logic addition for pbp parsing.
- More under the hood changes to adapt to tidyselect new version
  guidelines.
- [`load_phf_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_rosters.md)
  function added.
- [`load_nhl_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_rosters.md)
  function added.

## **fastRhockey 0.4.0**

CRAN release: 2022-10-25

- Updates logic to add Montreal Force to teams lists/parsing.
- Under the hood changes to adapt to tidyselect new version guidelines.

## **fastRhockey 0.3.1**

CRAN release: 2022-08-28

- Updates documentation per CRAN’s request.

## **fastRhockey 0.3.0**

CRAN release: 2022-03-25

- Add print method for all functions with a time stamp and description
  of the data.
- Add `phf_team_logos` dataset to package for reference.

## **fastRhockey 0.2.1**

- hotfix to `helper_phf_pbp_data()` penalty code.
- add [`try()`](https://rdrr.io/r/base/try.html) to function examples.

## **fastRhockey 0.2.0**

- [`espn_nhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_teams.md)
  function added.

## **fastRhockey 0.1.0**

CRAN release: 2021-12-10

- Prepped for CRAN.

## **fastRhockey 0.0.4**

#### Loader functions for PHF

- [`load_phf_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_pbp.md)
  function added.
- [`load_phf_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_team_box.md)
  function added.
- [`load_phf_player_box()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_player_box.md)
  function added.
- [`load_phf_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_schedule.md)
  function added.
- [`update_phf_db()`](https://fastRhockey.sportsdataverse.org/reference/update_phf_db.md)
  function added.

#### Player and Team Stats, Leaderboards

- [`phf_leaders()`](https://fastRhockey.sportsdataverse.org/reference/phf_leaders.md)
  function added.
- [`phf_standings()`](https://fastRhockey.sportsdataverse.org/reference/phf_standings.md)
  function added.
- [`phf_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/phf_player_stats.md)
  function added.
- [`phf_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_stats.md)
  function added.
- [`phf_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_roster.md)
  function added.

## **fastRhockey 0.0.3**

#### Function naming convention normalization

- `load_phf_game()` –\>
  [`phf_game_all()`](https://fastRhockey.sportsdataverse.org/reference/phf_game_all.md)
- [`load_phf_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_pbp.md)
  –\>
  [`phf_pbp()`](https://fastRhockey.sportsdataverse.org/reference/phf_pbp.md)
- `load_phf_boxscore()` –\>
  [`phf_team_box()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_box.md)
- `load_phf_raw_data()` –\>
  [`phf_game_raw()`](https://fastRhockey.sportsdataverse.org/reference/phf_game_raw.md)

## **fastRhockey 0.0.2**

- Added NHL functions from `powerplay` to `fastRhockey`.

## **fastRhockey 0.0.1**

- Added a `NEWS.md` file to track changes to the package.
