## Release summary

This is a minor release (1.1.0), an update to 1.0.0 (CRAN, 2026-08-25). It
adds 453 wrappers for 13 European leagues and ESPN men's and women's college
hockey, NHL conference/division and PWHL shift/xG loaders, and several fixes;
three output-type changes are listed under "Breaking changes" in NEWS.md.

## Test environments

* Local Windows 10 x64, R 4.6.0 (ucrt): `R CMD check --as-cran --timings
  --run-donttest` on the tarball built from a clean export (2026-11-09, an
  empty user .Renviron, NOT_CRAN unset) at db891864. The only later change
  to the package is the README (30149d9f); `--as-cran --no-examples
  --no-tests --no-manual` with the remote incoming checks on that commit
  was also `Status: OK`.
* GitHub Actions R-CMD-check: R release on macOS, Windows and Ubuntu,
  R-devel and oldrel-1 on Ubuntu. Green on main at 91c5c5c5 (2026-11-09).
  The two package commits after it change the help pages and the roxygen
  code that builds them; on the first of them (db891864) the release jobs
  on all three systems passed.
* R-hub v2 (linux, macos, macos-arm64 and windows, all R-devel): OK on
  2026-11-06 at c7918eec. That is an earlier commit; 4e7f6589 and 91c5c5c5
  changed R code after it.
* win-builder (devel and release): to be run before submission.

## R CMD check results

0 errors | 0 warnings | 0 notes

`Status: OK`. The incoming feasibility check, the tests (425 pass, 998 skip,
0 fail), the examples and the PDF and HTML manuals are OK. There is one INFO:

* Installed package size:

  ```
  installed size is  6.0Mb
  sub-directories of 1Mb or more:
    R      2.1Mb
    help   3.5Mb
  ```

  The help pages document 880 exported functions, and 729 of them carry a
  table of the columns they return; those tables are most of what remains.
  Since the 2026-11-05 check the size came down from 8.9 Mb: each
  `espn_wch_*()` function now shares its help page with its `espn_mch_*()`
  twin (121 pages instead of 242), and the seven largest families link to one
  family index page instead of listing every other member under "See also"
  (a list that grew with the square of the family size).

* No example ran over 5 s. With `--run-donttest` the slowest was
  `vhl_standings()` (1.96 s CPU, 4.82 s elapsed) in the full check, and 1.60 s
  CPU, 3.34 s elapsed when re-timed on its own.

* Offline: the same tarball with `--run-donttest --no-tests --no-manual` and
  the network blocked by a dead proxy raised no error; every example that
  needs the network fails inside its `try()`.

## Examples

* Every example that uses the network is in `\donttest{}` and wrapped in
  `try()`, so it fails gracefully without network access. The only examples
  outside `\donttest{}` are `most_recent_nhl_season()`,
  `most_recent_nhl_season_api_param()` and `most_recent_pwhl_season()`,
  which compute a season from the date.
* 39 examples run only in interactive sessions (`@examplesIf interactive()`,
  still inside `\donttest{}` and `try()`):
  * All 17 Swiss National League examples (`nl_*()`). The functions read
    www.nationalleague.ch/api, and that site's robots.txt disallows `/api`,
    so we do not want CRAN's automated checks to request it. The help pages
    say so, and users who call the functions do so knowingly.
  * Three loaders that download a full season of play-by-play:
    `load_nhl_pbp()`, `load_nhl_pbp_full()` and `load_nhl_pbp_lite()`.
  * 19 whose host is slow from automated environments or whose endpoint
    returns a season-wide payload it cannot narrow: `ha_games()`,
    `ha_schedule()`, `liiga_teams()`, `nhl_records_draft_prospect()`,
    `nhl_all_players_by_season()` (one request per team),
    `espn_nhl_standings_core()`, `espn_nhl_standings()`,
    `espn_nhl_player_splits()`, `icehl_league_part_player_stats()`,
    `icehl_schedule()` and nine `vhl_*()` examples (vhlru.ru). Re-timed one
    at a time from a home connection on 2026-11-09, the slowest were
    `ha_games()` (1.5 s CPU, 44.9 s elapsed), `espn_nhl_standings_core()`
    (15.5 s CPU, 16.6 s), `nhl_records_draft_prospect()` (8.1 s, 12.3 s),
    `liiga_teams()` (7.2 s, 10.5 s) and `icehl_league_part_player_stats()`
    (8.5 s, 9.5 s).
* Three more examples keep a `--run-donttest` call and put only their slow
  calls behind `@examplesIf interactive()`: the parsed calls of
  `nhl_game_feed()` and `nhl_game_pbp()` (the first parsed game downloads
  about 4 MB of xG models to the user cache; the `raw = TRUE` calls still
  run), and the playoff and data-index calls of `nhl_schedule()`.
* Three examples are in `\dontrun{}` because they cannot run unattended:
  `update_pwhl_db()` writes to a database, `refresh_xg_models()` downloads
  model files to the user cache, and `helper_nhl_calculate_xg()` needs those
  downloaded xgboost models. `update_nhl_db()`, which downloads every season
  and writes it to a database, has no example.

## Defunct PHF functions

The Premier Hockey Federation ceased operations. Its 21 exported functions
(the 14 `phf_*()` scrapers, the five `load_phf_*()` loaders,
`update_phf_db()` and `most_recent_phf_season()`) were made defunct in 1.0.0:
each one's first line is `lifecycle::deprecate_stop()`, so a call raises a
`lifecycle_error_deprecated` error and never reaches the network. They have
no examples. They stay exported so that old code gets that informative error
instead of "could not find function"; the archived PHF data lives in the
fastRhockey-data GitHub repository.

## Tests

Every test that needs the network is skipped on CRAN (`skip_on_cran()` plus
an environment variable per source). The tests that run on CRAN read
committed fixtures and mock the HTTP layer.

## Reverse dependencies

There are no reverse dependencies on CRAN
(`tools::package_dependencies("fastRhockey", reverse = TRUE, db = available.packages(), which = "all")`
is empty, 2026-11-09).

## Internet resources

All functions that access internet resources (the NHL, PWHL and HockeyTech
APIs, ESPN, Fox Sports, the European league sites, and the pre-built season
datasets on GitHub release assets) fail gracefully with an informative
message or a classed condition when the resource is unavailable.
