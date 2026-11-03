## Release summary

This is a minor release (1.1.0) on top of 1.0.0 (CRAN, 2026-08-25). The
full list of changes is in NEWS.md. The main additions:

* European hockey: 13 new league families with 211 wrappers. They
  cover Germany's DEL, Austria's ICE Hockey League, the United Kingdom's
  EIHL, Denmark's Metal Ligaen, the KHL, Russia's VHL, Sweden's SHL and
  HockeyAllsvenskan, Finland's Liiga, Switzerland's National League and
  Swiss Ice Hockey Federation competitions, Czechia's Extraliga and France's
  Ligue Magnus. Each family waits at least 1 s between requests to a host,
  retries 429/5xx answers honouring `Retry-After`, and raises classed
  conditions (`fastRhockey_no_data`, `fastRhockey_asset_fetch`,
  `fastRhockey_bad_request`) instead of returning an empty result.
* NCAA men's and women's hockey from ESPN (`espn_mch_*()`, `espn_wch_*()`):
  242 wrappers, 121 per league. With the European families that is the 453
  new wrappers listed in NEWS.md.
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
* Fifteen networked examples run only in interactive sessions
  (`@examplesIf interactive()`, still inside `\donttest{}` and `try()`):
  `ha_games()`, `liiga_teams()`, `nhl_records_draft_prospect()`,
  `espn_nhl_standings_core()`, `load_nhl_pbp()`, `load_nhl_pbp_full()` and
  nine `vhl_*()` functions (`vhl_schedule()`, `vhl_game_info()`,
  `vhl_goals()`, `vhl_penalties()`, `vhl_player_box()`, `vhl_goalie_box()`,
  `vhl_pbp()`, `vhl_skater_stats()`, `vhl_goalie_stats()`). Each took about
  6 to 32 seconds in at least one of two GitHub Actions runner samples.
  Five of the vhlru.ru examples took 4 to 12 times longer from the runner
  than from a home connection, and `ha_games()` (hockeyallsvenskan.se) took
  32 s in one runner sample, 5 s in the other and 3 s locally. Every
  other function in those families keeps a `--run-donttest` example.
* Three examples are in `\dontrun{}` because they cannot run unattended:
  `update_pwhl_db()` writes to a database, `refresh_xg_models()` downloads
  model files to the user cache, and `helper_nhl_calculate_xg()` needs those
  downloaded xgboost models.

## Tests

Every test that needs the network is skipped on CRAN (`skip_on_cran()` plus
an environment variable per source). The tests that run on CRAN read
committed fixtures and mock the HTTP layer.

## R CMD check results

0 errors | 0 warnings | 0 notes

* Local Windows 10, R 4.6.0, `R CMD check --as-cran` on the built tarball
  (2026-11-03; NOT_CRAN unset, so every networked test skips): Status: OK.
  CRAN incoming feasibility, the examples, the examples with
  `--run-donttest` (9 minutes), the tests (17 seconds) and the PDF and HTML
  manuals are all OK.
* Installed size. R 4.6.0 on Windows reports it as INFO; platforms that still
  NOTE packages over 5 MB may show it. The tarball is 2.2 MB and the
  installed size is 9.1 MB (R 2.1 MB, help 6.6 MB), because the help pages
  document about 880 exported functions, most with a column table for the
  data they return.
* GitHub Actions (R release on Ubuntu, macOS and Windows): see the R-CMD-check
  workflow on the release commit.
* R-hub: pending.

## Reverse dependencies

There are no reverse dependencies on CRAN
(`tools::package_dependencies("fastRhockey", reverse = TRUE, db = available.packages())`
is empty).

## Internet resources

All functions that access internet resources (the NHL, PWHL and HockeyTech
APIs, ESPN, Fox Sports, the European league sites, and the pre-built season
datasets on GitHub release assets) fail gracefully with an informative
message or a classed condition when the resource is unavailable.
