## Release summary

This is a minor release (1.1.0) on top of 1.0.0 (CRAN, 2026-08-25). The
full list of changes is in NEWS.md. The main additions:

* European hockey: 13 new league families with about 210 wrappers. They
  cover Germany's DEL, Austria's ICE Hockey League, the United Kingdom's
  EIHL, Denmark's Metal Ligaen, the KHL, Russia's VHL, Sweden's SHL and
  HockeyAllsvenskan, Finland's Liiga, Switzerland's National League and
  Swiss Ice Hockey Federation competitions, Czechia's Extraliga and France's
  Ligue Magnus. Each family waits at least 1 s between requests to a host,
  retries 429/5xx answers honouring `Retry-After`, and raises classed
  conditions (`fastRhockey_no_data`, `fastRhockey_asset_fetch`,
  `fastRhockey_bad_request`) instead of returning an empty result.
* NCAA men's and women's hockey from ESPN (`espn_mch_*()`, `espn_wch_*()`).
* NHL conference and division reference loaders (`load_nhl_groups()` and
  friends), PWHL shift and expected-goals loaders, and a `detailed` option
  for `nhl_game_shifts()`.
* Fixes: the SHL wrappers read the API's renamed season directory key;
  `pwhl_standings()` honours `season`; the NHL stats REST and playoff
  wrappers no longer request a season that has not started; the PWHL and NHL
  release loaders fall back to the previous season when the default season
  is not published yet.
* Dependencies: Rcpp and RcppParallel (unused; the package has no compiled
  code) are no longer imported, and furrr, future, ggplot2, ggrepel,
  rmarkdown, stringi and usethis (all unused) are no longer suggested.

## Examples

* Every example that uses the network is in `\donttest{}` and wrapped in
  `try()`, so it fails gracefully without network access. The only examples
  outside `try()` are `most_recent_nhl_season()`,
  `most_recent_nhl_season_api_param()` and `most_recent_pwhl_season()`,
  which compute a season from the date.
* Four examples are in `\dontrun{}` (see the check results below).

## Tests

Every test that needs the network is skipped on CRAN (`skip_on_cran()` plus
an environment variable per source). The tests that run on CRAN read
committed fixtures and mock the HTTP layer.

## R CMD check results

0 errors | 0 warnings | 1 note

* Local Windows 10, R 4.6.1, `R CMD check --as-cran` on the built tarball
  (2026-11-01; NOT_CRAN unset, so every networked test skips): 0 errors,
  0 warnings, 1 note. The note is

      checking for non-standard things in the check directory ... NOTE
      Found the following files/directories:
        ''NULL''

  That `'NULL'` folder is left in the check directory by this machine's R
  startup profile, not by the package; it does not appear on other machines.
  A second run showed an HTML-manual note for three unescaped
  angle-bracket placeholders (`khl_player_profile`, `magnus_teams`). Those
  pages are fixed, and a re-run (`--no-examples --no-tests`) builds the PDF
  and HTML manuals cleanly with Status: OK.
  The tarball is 2.2 MB; the installed size is 8.9 MB (R 2.1 MB, help
  6.4 MB), because the help pages document about 880 exported functions,
  most with a column table for the data they return. The examples with
  `--run-donttest` pass in 11 minutes and the tests in 21 seconds.
* The database and model-download examples (`update_pwhl_db()`,
  `refresh_xg_models()`, `helper_nhl_calculate_xg()`, which needs the
  downloaded xgboost models) and `nhl_game_boxscore()` are in `\dontrun{}`.

* GitHub Actions (R release on Ubuntu, macOS and Windows): see the PR checks.
* win-builder: TODO
* R-hub: TODO

## Reverse dependencies

There are no reverse dependencies on CRAN
(`tools::package_dependencies("fastRhockey", reverse = TRUE, db = available.packages())`
is empty).

## Internet resources

All functions that access internet resources (the NHL, PWHL and HockeyTech
APIs, ESPN, Fox Sports, the European league sites, and the pre-built season
datasets on GitHub release assets) fail gracefully with an informative
message or a classed condition when the resource is unavailable.
