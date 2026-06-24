# CLAUDE.md — fastRhockey

R package for tidy hockey data: live scrapers + pre-compiled loaders for the
**NHL** and **PWHL**, plus ESPN/Fox NHL wrappers and four CHL/AHL HockeyTech
leagues. Part of the [SportsDataverse](https://sportsdataverse.org) family
(siblings: `hoopR`, `wehoop`, `cfbfastR`, `baseballr`, `sportsdataverse-py`).
Docs: <https://fastRhockey.sportsdataverse.org>. Companion data repos:
`fastRhockey-nhl-data` / `fastRhockey-pwhl-data` (release source for `load_*`),
and the legacy `fastRhockey-data` (PHF archive + older NHL JSON).

- License MIT | R (>= 4.1.0) | roxygen2 markdown | testthat edition 3 | v1.0.0

## Commands

```r
devtools::document()                 # roxygen2 -> man/ + NAMESPACE
devtools::test()                     # testthat (set NOT_CRAN=true for full run)
testthat::test_file("tests/testthat/test-nhl_standings.R")
devtools::check()                    # R CMD check
pkgdown::build_site()
```

CI (`.github/workflows/`): `R-CMD-check.yaml` (macOS/Windows release + Ubuntu
release & oldrel-1), `pkgdown.yaml`, `rhub.yaml`. Released on CRAN + r-universe.

## Function families & data sources

Live wrappers use **httr2** (`req_perform` / `req_retry`, ~17 call sites — there
is no `httr::RETRY` here); responses go through `janitor::clean_names()`.

| Family | Prefix(es) | Source | Files |
|---|---|---|---|
| NHL Web API | `nhl_*` | `api-web.nhle.com/v1/` | `nhl_*.R`, `helpers_nhl.R` |
| NHL Edge | `nhl_edge_*`, `nhl_cat_edge_*` (35) | `api-web.nhle.com/v1/edge/*` & `/cat/edge/*` | `helpers_nhl_edge.R` |
| NHL Stats REST | `nhl_stats_*` | `api.nhle.com/stats/rest/` (Cayenne filters) | (httr2 direct) |
| NHL Records | `nhl_records_*` (25) | `records.nhl.com/site/api/` | `helpers_nhl_records.R` |
| ESPN NHL | `espn_nhl_*` (126) | `sports.core.api.espn.com` / `site.api.espn.com` / `site.web.api.espn.com` | `espn_nhl_*.R` |
| Fox NHL | `fox_nhl_*` (8) | `api.foxsports.com/bifrost/v1/nhl/` | `fox_nhl.R` |
| PWHL | `pwhl_*` (35) | HockeyTech (`lscluster.hockeytech.com`) | `pwhl_*.R`, `pwhl_helpers.R` |
| HockeyTech CHL/AHL | `ahl_*` `ohl_*` `whl_*` `qmjhl_*` (12 each) | HockeyTech | `hockeytech_*.R` |
| PHF (deprecated) | `phf_*` | ESPN (league defunct) | `phf_*.R` |

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

- `load_nhl_*` (27, incl. parity aliases) share `.nhl_release_loader()` in
  `nhl_loaders.R`; `load_pwhl_*` (20) share `.pwhl_release_loader()` in
  `pwhl_loaders.R`. Both validate seasons, build release URLs from a
  `(release_tag, file_prefix)` catalog row, download in parallel (optional
  `progressr`), optionally write to a `DBIConnection`, and tag output
  `fastRhockey_data`. Add a dataset = one catalog row + a thin wrapper.
  DB helpers: `update_nhl_db()` / `update_pwhl_db()` / `update_phf_db()`.
  `load_phf_*` remain exported (data archived); the live `phf_*()` scrapers
  emit `lifecycle` deprecation.
- **xG**: 3 XGBoost models (5v5 / special-teams / penalty-shot) downloaded on
  `.onLoad()` from `fastRhockey-nhl-data/main/models/`, cached via
  `tools::R_user_dir("fastRhockey","cache")`, held in `.xg_env` (`zzz.R`).
  `refresh_xg_models()` re-pulls; requires the `xgboost` Suggests package.

## Conventions

- One exported fn per file; filename matches the function name. Internal
  helpers are `.dotted` (`.pwhl_api()`, `.nhl_edge_api()`).
- Season formats: NHL = `"20242025"` (concatenated); PWHL/HockeyTech = end-year
  integer. `most_recent_<league>_season()` returns the concluding year;
  `most_recent_nhl_season_api_param()` returns the `"20242025"` form.
- All returns wrap in S3 class `fastRhockey_data` via
  `make_fastRhockey_data()` — tests assert `expect_s3_class(x, "fastRhockey_data")`.
- Conventional Commits; scopes: nhl, nhl-edge, nhl-records, nhl-stats, espn,
  fox, hockeytech, pwhl, phf, xg, loader, pkgdown, ci.
- **Never** add AI co-author/author trailers to commits.

## Testing

- `skip_on_cran()` + `skip_<league>_test()` from `tests/testthat/helper-skip.R`,
  gated by `RUN_NHL_TESTS` / `RUN_PHF_TESTS` / `RUN_PWHL_TESTS` (CI sets all
  three `true`). Assert `data.frame` + `fastRhockey_data`, `nrow > 0`, columns.
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
