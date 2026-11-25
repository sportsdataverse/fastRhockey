## Release summary

This is a minor release (1.1.0), an update to 1.0.0 (CRAN, 2026-08-25). It
adds 453 wrappers for 13 European leagues and ESPN men's and women's college
hockey, four NHL conference/division reference loaders, and several fixes;
four changes to returned output are listed under "Breaking changes" in
NEWS.md.

## Test environments

The package checked is commit ef32934d3 (`ef32934d352a720cb014f2a73bd2b0cf9acd629d`, the R
code of the 1.1.0 release as of 2026-11-25). The one commit after it changes
only this file, which `.Rbuildignore` excludes, so the tarball built at the
current commit holds the same files as the one checked. A result from an
older commit names that commit.

* Local Windows 10 x64, R 4.6.0 (ucrt), at ef32934d3: `R CMD check --as-cran
  --timings --run-donttest` on the tarball built from a clean `git archive`
  export (2026-11-25, an empty user .Renviron, NOT_CRAN unset): 1 NOTE
  (example timing, see below).
* GitHub Actions R-CMD-check at ef32934d3
  (https://github.com/sportsdataverse/fastRhockey/actions/runs/45554387152),
  R 4.6.2 release: `Status: OK` on ubuntu-latest and on macos-latest. The
  macOS job failed once in its dependency-install step (a pak metadata
  error, before the package was built) and passed on a re-run. Windows
  release and Ubuntu R-devel run only in the weekly and manual CI and were
  not run at ef32934d3.
* win-builder (devel and release): not yet run (win-builder unavailable);
  to be run before submission.
* R-hub v2, R-devel, at f5a8543a4, an earlier commit of the release branch,
  before the R changes of #105, #106 and #107
  (https://github.com/sportsdataverse/fastRhockey/actions/runs/45455257204):
  `Status: OK` on linux, windows, macos and macos-arm64. Not re-run at
  ef32934d3.

## R CMD check results

0 errors | 0 warnings | 1 note (local R 4.6.0, ef32934d3)

The incoming feasibility check, the tests (704 pass, 1311 skip, 0 fail),
the PDF and HTML manuals and everything else are OK. The NOTE:

```
* checking examples ... NOTE
Examples with CPU (user + system) or elapsed time > 5s
                user system elapsed
khl_game_header 0.14      0       6
```

`khl_game_header()` is a `\donttest{}` example that makes one request to
the KHL site. It used 0.14 s of CPU and spent the rest waiting on the
network. Re-run three times from the same machine just after the check, the
call took 2.4, 1.0 and 3.0 s elapsed (0.01 to 0.11 s CPU). The longest CPU
time of any example was `liiga_games()` (3.07 s CPU, 4.51 s elapsed).

There is one INFO:

* Installed package size:

  ```
  installed size is  6.2Mb
  sub-directories of 1Mb or more:
    R      2.1Mb
    help   3.6Mb
  ```

  The GitHub Actions checks of ef32934d3 report 7.6 Mb (Ubuntu) and 8.0 Mb
  (macOS), so the Linux flavours may show the installed-size NOTE. The help
  pages document 880 exported functions, and 729 of them carry a table of the
  columns they return; those tables are most of the help. Since the
  2026-11-05 check the local size came down from 8.9 Mb: each `espn_wch_*()`
  function now shares its help page with its `espn_mch_*()` twin (121 pages
  instead of 242), and the seven largest families link to one family index
  page instead of listing every other member under "See also" (a list that
  grew with the square of the family size).

* Offline, at 31b80d376 (older): the tarball checked on R 4.6.0 with
  `--run-donttest --no-tests --no-manual` and the network blocked by a dead
  proxy (2026-11-24) gave `Status: OK`; every example that needs the network
  fails inside its `try()`. Not re-run at ef32934d3. The R changes since
  reject a missing ESPN id before any request, parse flat college rosters,
  re-raise a parsing warning (named for the function) instead of returning an
  empty result, and name the HTTP status and URL in request errors; none adds
  a request.

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
  about 3 MB of xG models to the user cache, or to `tempdir()` under
  R CMD check; the `raw = TRUE` calls still run), and the playoff and
  data-index calls of `nhl_schedule()`.
* Three examples are in `\dontrun{}` because they cannot run unattended:
  `update_pwhl_db()` writes to a database, `refresh_xg_models()` downloads
  model files to the user cache, and `helper_nhl_calculate_xg()` needs the
  xgboost package and a play-by-play frame from a network call, and
  downloads the models on first use. `update_nhl_db()`, which downloads
  every season and writes it to a database, has no example.

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
