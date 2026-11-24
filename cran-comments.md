## Release summary

This is a minor release (1.1.0), an update to 1.0.0 (CRAN, 2026-08-25). It
adds 453 wrappers for 13 European leagues and ESPN men's and women's college
hockey, four NHL conference/division reference loaders, and several fixes;
four changes to returned output are listed under "Breaking changes" in
NEWS.md.

## Test environments

All results below are for the package at commit 31b80d376. The later
commits change only this file, which is build-ignored, so the tarball is the
same.

* Local Windows 10 x64, R 4.6.0 (ucrt): `R CMD check --as-cran --timings
  --run-donttest` on the tarball built from a clean `git archive` export
  (2026-11-24, an empty user .Renviron, NOT_CRAN unset): `Status: OK`.
* Local Windows 10 x64, R 4.6.1 (ucrt), same tarball and options
  (2026-11-24): 1 NOTE, from the check environment rather than the package
  (see below).
* win-builder (devel and release): not yet run (win-builder unreachable
  2026-11-24); to be run before submission.
* R-hub v2 (linux, windows, macos, macos-arm64): dispatched 2026-11-24 at
  31b80d376 (https://github.com/sportsdataverse/fastRhockey/actions/runs/45429874836),
  results pending.
* GitHub Actions R-CMD-check (R release on macOS, Windows and Ubuntu,
  R-devel and oldrel-1 on Ubuntu): runs on the release pull request; results
  pending at the time of writing. The last completed run, on df436f80d (the
  commit this release-prep branch starts from; the branch changes help text,
  NEWS, the citation and build metadata, no R code), passed on all five.

## R CMD check results

0 errors | 0 warnings | 0 notes (R 4.6.0)

`Status: OK`. The incoming feasibility check, the tests (479 pass, 1306 skip,
0 fail), the examples and the PDF and HTML manuals are OK. On R 4.6.0 the
incoming check printed a transient warning while downloading CRAN's
`packages.rds` and then passed; on the R 4.6.1 run it passed without one.

The R 4.6.1 run gave one NOTE:

```
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
```

The `'NULL'` directories are created in the check directory and in `tests/`
as each R process starts, before any package code runs. A minimal package
with one function, one example and one test gives the same NOTE under R 4.6.1
`--as-cran` on this machine, and none under R 4.6.0 or without `--as-cran`,
so we read it as a local check-environment issue.

There is one INFO:

* Installed package size:

  ```
  installed size is  6.2Mb
  sub-directories of 1Mb or more:
    R      2.1Mb
    help   3.6Mb
  ```

  The GitHub Actions checks of df436f80d (Linux, macOS and Windows) report
  7.5 to 7.9 Mb (Linux: R 3.7 Mb, help 3.5 Mb), so the Linux flavours may
  show the installed-size NOTE. The help pages document 880 exported
  functions, and 729 of them carry a table of the columns they return; those
  tables are most of the help. Since the 2026-11-05 check the local size came
  down from 8.9 Mb: each `espn_wch_*()` function now shares its help page with
  its `espn_mch_*()` twin (121 pages instead of 242), and the seven largest
  families link to one family index page instead of listing every other
  member under "See also" (a list that grew with the square of the family
  size).

* No example ran over 5 s. With `--run-donttest` on R 4.6.0 the slowest was
  `ha_skater_stats()` (1.85 s CPU, 3.89 s elapsed); the longest elapsed was
  `elh_game_ids()` (0.75 s CPU, 4.41 s elapsed).

* Offline: the same tarball checked on R 4.6.0 with `--run-donttest
  --no-tests --no-manual` and the network blocked by a dead proxy
  (2026-11-24) gave `Status: OK`; every example that needs the network fails
  inside its `try()`.

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
  about 3 MB of xG models to the user cache; the `raw = TRUE` calls still
  run), and the playoff and data-index calls of `nhl_schedule()`.
* Three examples are in `\dontrun{}` because they cannot run unattended:
  `update_pwhl_db()` writes to a database, `refresh_xg_models()` downloads
  model files to the user cache, and `helper_nhl_calculate_xg()` needs those
  downloaded xgboost models. `update_nhl_db()`, which downloads every season
  and writes it to a database, has no example.

## xG model files

`library(fastRhockey)` makes no network call; there is no `.onLoad()`. The
NHL xG models are downloaded on the first call that needs them
(`helper_nhl_calculate_xg()`, and so `nhl_game_pbp()` / `nhl_game_feed()`)
from a GitHub release, together with a small manifest (`xg_model_meta.json`)
that lists each file's sha256. Files are kept in
`tools::R_user_dir("fastRhockey", "cache")`, or in `tempdir()` when
`_R_CHECK_PACKAGE_NAME_` is set (under R CMD check). Each session re-reads the
manifest; a cached file that does not match it is replaced, and offline the
last manifest seen is used. A failed download makes
`helper_nhl_calculate_xg()` raise an informative error; `nhl_game_pbp()` /
`nhl_game_feed()` catch it, give a message and return `xg = NA`.

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
is empty, 2026-11-24).

## Internet resources

All functions that access internet resources (the NHL, PWHL and HockeyTech
APIs, ESPN, Fox Sports, the European league sites, and the pre-built season
datasets on GitHub release assets) fail gracefully with an informative
message or a classed condition when the resource is unavailable.
