# CLAUDE.md — fastRhockey

R package for tidy hockey data: live scrapers + pre-compiled loaders for the
**NHL** and **PWHL**, plus ESPN/Fox NHL wrappers, four CHL/AHL HockeyTech
leagues, thirteen European leagues and ESPN men's/women's college hockey.
Part of the [SportsDataverse](https://sportsdataverse.org) family
(siblings: `hoopR`, `wehoop`, `cfbfastR`, `baseballr`, `sportsdataverse-py`).
Docs: <https://fastRhockey.sportsdataverse.org>. Companion data repos:
`fastRhockey-nhl-data` / `fastRhockey-pwhl-data` (release source for `load_*`),
and the legacy `fastRhockey-data` (PHF archive + older NHL JSON).

- License MIT | R (>= 4.1.0) | roxygen2 8.1.0 markdown | testthat edition 3 |
  CRAN 1.0.0, next release 1.1.0
- Rule: live (network) tests always skip on CRAN and CI; run them locally. Offline
  tests (mocks, fixtures, captured bodies) run everywhere. Gate every new live
  test with its `skip_<family>_test()` helper (`helper-skip.R`
  `skip_live_test()`: skips on CRAN and CI, then runs only when the family's
  `RUN_*_TESTS` toggle is `1` or `true`; unset means skip).
- HockeyTech JSONP is stripped ONLY via `.hockeytech_api()`
  (`hockeytech_helpers.R`) — never per-function regex on the callback tail
  (the tail shape varies by payload and has silently emptied results).
- sdv-py parity note: Python carries 20 HockeyTech league families
  (echl/ushl/sphl/... via `build_family`); R carries pwhl + ahl/ohl/whl/qmjhl.
  The 15-league gap is a known, deliberate divergence — port via the
  registry pattern if ever needed, not ad hoc.

## Commands

```r
devtools::document()                 # roxygen2 -> man/ + NAMESPACE
devtools::test()                     # offline tests; live ones need RUN_*_TESTS=1
testthat::test_file("tests/testthat/test-nhl_standings.R")
devtools::check()                    # R CMD check
pkgdown::build_site()
```

CI (`.github/workflows/`): `R-CMD-check.yaml` (macOS/Windows release + Ubuntu
devel, release & oldrel-1; devel is non-blocking), `test-coverage.yaml`,
`urlchecker.yaml`, `pkgdown.yaml`, `rhub.yaml`, `star-history.yml`. Released on
CRAN + r-universe.

## Function families & data sources

Live wrappers use **httr2**; there is no `httr` dependency. NHL / PWHL /
HockeyTech / ESPN / Fox wrappers call `.retry_request(url, params = ...)`
(`utils.R`: `httr2::req_retry()`, non-2xx do not throw), then `check_status()`
and `.resp_json()` / `.resp_text()`. Three failure contracts: NHL and PWHL
wrappers message and return `NULL`; ESPN (NHL + college), Fox and AHL/OHL/
WHL/QMJHL wrappers report via `.report_api_error()` / `cli` and return an
empty data frame; the European wrappers build with `.build_request()`,
perform through `.polite_perform()` / `.euro_get()` (`euro_hockey_runtime.R`:
at least 1 s between requests per host; only 429/500/502/503/504 and
connection failures are retried, honouring `Retry-After` up to 120 s) and
raise classed conditions: `fastRhockey_no_data` (404 / nothing there),
`fastRhockey_bad_request` (400 / 422), `fastRhockey_asset_fetch` (any other
failure, including an exhausted 429), all inheriting `fastRhockey_error`.
Responses go through `janitor::clean_names()`.

| Family | Prefix(es) | Source | Files |
|---|---|---|---|
| NHL Web API | `nhl_*` | `api-web.nhle.com/v1/` | `nhl_*.R`, `helpers_nhl.R` |
| NHL Edge | `nhl_edge_*`, `nhl_cat_edge_*` (35) | `api-web.nhle.com/v1/edge/*` & `/cat/edge/*` | `helpers_nhl_edge.R` |
| NHL Stats REST | `nhl_stats_*` | `api.nhle.com/stats/rest/` (Cayenne filters) | (httr2 direct) |
| NHL Records | `nhl_records_*` (25) | `records.nhl.com/site/api/` | `helpers_nhl_records.R` |
| ESPN NHL | `espn_nhl_*` (126) | `sports.core.api.espn.com` / `site.api.espn.com` / `site.web.api.espn.com` | `espn_nhl_*.R` |
| Fox NHL | `fox_nhl_*` (8) | `api.foxsports.com/bifrost/v1/nhl/` | `fox_nhl.R` |
| PWHL | `pwhl_*` (22) | HockeyTech (`lscluster.hockeytech.com`) | `pwhl_*.R`, `pwhl_helpers.R` |
| HockeyTech CHL/AHL | `ahl_*` `ohl_*` `whl_*` `qmjhl_*` (12 each) | HockeyTech | `ahl.R` `ohl.R` `whl.R` `qmjhl.R`, `hockeytech_*.R` |
| European | `del_` `icehl_` `eihl_` `metal_` (eSports), `khl_` `vhl_` `shl_` `ha_` `liiga_` `sihf_` `nl_` `elh_` `magnus_` (211) | each league's public API | `esports*.R`, `khl.R`, `vhl.R`, `shl.R`, `ha.R`, `liiga.R`, `sihf.R`, `nationalleague.R`, `elh.R`, `magnus.R`, `euro_hockey_runtime.R` |
| ESPN college | `espn_mch_*`, `espn_wch_*` (121 each) | ESPN (`mens-` / `womens-college-hockey`) | `espn_ncaa_hockey.R` |
| PHF (defunct) | `phf_*`, `load_phf_*` | league defunct; every function stops (`deprecate_stop`) | `phf_*.R` |

- **NHL Edge** `.nhl_edge_api()` handles the `/now` vs `/{season}/{gameType}`
  URL split; `.nhl_edge_to_df()` normalizes the assorted shapes.
- **NHL Stats** `leaders/{skaters,goalies}/{attr}` rejects `start`/`limit`
  (500); valid goalie attrs are `savePctg`, `gaa`, `shutouts` only.
- **NHL Records** does not support path-suffix filtering (`franchise/{id}` →
  404) — filter with `cayenneExp`. Tabular shape is `{data:[...], total:N}`.
- **HockeyTech** returns JSONP (Angular callback) that is regex-stripped before
  parse. The 5-league registry (keys, league/site ids, pbp style) lives in
  `hockeytech_leagues.R`; `SDV_<LEAGUE>_API_KEY` env var overrides the key.
- **PWHL/HockeyTech analytics**: `*_game_shifts/_player_toi/_game_corsi`.
  Corsi/Fenwick are proxies — the feed has no missed-shot event.

## Loaders & xG

- `load_nhl_*` (31: 23 canonical + 8 parity aliases). The 19 season loaders
  and the 8 aliases share `.nhl_release_loader()` in `nhl_loaders.R` (RDS
  assets); the 4 conference/division loaders (`load_nhl_groups()`,
  `_group_seasons()`, `_group_aliases()`, `_team_group_seasons()`) use
  `.groups_release_loader()` in `nhl_group_loaders.R` (CSV assets from the
  `nhl_groups` release). `load_pwhl_*` (22: 17 canonical + 5 aliases, incl. `load_pwhl_shifts` +
  `load_pwhl_xg_pbp`) share `.pwhl_release_loader()` in
  `pwhl_loaders.R`. Every hockey tag on the sportsdataverse-data releases
  has a loader (verified 2026-08-25); `nhl_xg_models` is model artifacts,
  not a dataset. Both validate seasons, build release URLs from a
  `(release_tag, file_prefix)` catalog row, download in parallel (optional
  `progressr`), optionally write to a `DBIConnection`, and tag output
  `fastRhockey_data`. Add a dataset = one catalog row + a thin wrapper.
  DB helpers: `update_nhl_db()` / `update_pwhl_db()` / `update_phf_db()`.
  `load_phf_*`, `update_phf_db()` and the `phf_*()` scrapers remain exported
  but are defunct since 1.0.0 (`lifecycle::deprecate_stop()`); the archived
  PHF data lives in the `fastRhockey-data` repo.
- **xG**: 3 XGBoost models (5v5 / special-teams / penalty-shot) loaded on first
  use by `helper_nhl_calculate_xg()` (never in `.onLoad()`, CRAN policy) from the
  sportsdataverse-data `nhl_xg_models` release, sha256-pinned in `.xg_pins`
  (`zzz.R`; a retrain ships by updating the pins), cached via
  `tools::R_user_dir("fastRhockey","cache")` (a mismatched cached file is
  evicted), held in `.xg_env`. `refresh_xg_models()` re-pulls; requires the
  `xgboost` Suggests package.

## Conventions

- One exported fn per file; filename matches the function name. Internal
  helpers are `.dotted` (`.pwhl_api()`, `.nhl_edge_api()`).
- Season formats: NHL = `"20242025"` (concatenated); PWHL/HockeyTech = end-year
  integer. `most_recent_<league>_season()` returns the concluding year;
  `most_recent_nhl_season_api_param()` returns the `"20242025"` form.
- All returns wrap in S3 class `fastRhockey_data` via
  `make_fastRhockey_data()` — tests assert `expect_s3_class(x, "fastRhockey_data")`.
- Conventional Commits; scopes: nhl, nhl-edge, nhl-records, nhl-stats, espn,
  fox, hockeytech, pwhl, phf, euro (or the league prefix: khl, shl, ...), xg,
  loader, pkgdown, ci.
- **Never** add AI co-author/author trailers to commits.

## Testing

- Gate live tests with `skip_<family>_test()` from
  `tests/testthat/helper-skip.R`. Each goes through `skip_live_test()`:
  `skip_on_cran()`, `skip_on_ci()`, then skip unless the toggle is `1` or
  `true` (any case); unset means skip. Toggles: `RUN_NHL_TESTS`,
  `RUN_PWHL_TESTS`, `RUN_ESPN_TESTS` (ESPN NHL), `RUN_EURO_TESTS` (European
  leagues and ESPN college hockey), `RUN_PHF_TESTS`, and `RUN_SLOW_NHL_TESTS`
  (`skip_slow_nhl_test()`, the two slow season-wide NHL aggregator tests, on
  top of `RUN_NHL_TESTS`). CI passes the toggles from repository variables
  but `skip_on_ci()` still skips, so set them in your own session, e.g.
  `Sys.setenv(NOT_CRAN = "true", RUN_EURO_TESTS = "1")`. Assert
  `data.frame` + `fastRhockey_data`, `nrow > 0`, columns.
- Offline replay: `test-<source>_offline.R` are generated from
  `data-raw/http_fixtures/calls.txt` (`record_fixtures.R` records the httr2
  responses into `tests/testthat/fixtures/http/`, incrementally; then
  `generate_tests.R`). Pass seasons/dates explicitly, end a call that needs the
  whole payload with `# keep`. The fixtures are `.Rbuildignore`d and the tests
  skip without them. Every request must go through httr2 (`.retry_request()`,
  `.read_json_url()`), or the replay cannot answer it. PHF is in `.covrignore`.
- xG tests add `skip_if_not_installed("xgboost")`; deprecated-fn tests use
  `suppressWarnings()`. Use fixed game ids for reproducibility
  (e.g. `game_id = 2023020001`, PWHL `game_id = 27`).

## Reference

- `data-raw/` holds the NHL endpoint catalog + OpenAPI 3.0.3 specs
  (`nhl_*_openapi.{json,yaml}`) for api-web / stats-rest / records, regenerated
  by `_gen_openapi.py` from the upstream `dfleis/nhl-api-docs` catalog
  (working `*.md` notes are untracked).
- `_pkgdown.yml` reference index is hand-grouped by family — add new exports to
  the matching section.

## Cheat sheet

There is a printable one-page reference for this package at
<https://sportsdataverse.org/cheatsheets/fastRhockey.pdf>, one of [a set covering every SportsDataverse package](https://sportsdataverse.org/cheatsheets).
Keep it in mind when adding or renaming an exported function: the sheet is a
hand-built canvas, so a surface change means the sheet needs a revision too.
