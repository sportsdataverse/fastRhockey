## Release summary

This is version 1.0.0 that signals API stability for the NHL and PWHL
endpoints. Key changes:

* Formally deprecated all PHF functions via `lifecycle` (league ceased
  operations).
* Consolidated new NHL API functions into existing NHL functions rather than creating `_v2` variants and deprecating the existing ones since the original API endpoints were deprecated.
* Added `lifecycle` package as an Imports dependency for formal deprecation
  management.
* Fixed NAMESPACE.
* Fixed `nhl_draft_year()` to use updated NHL API endpoint format.
* Fixed `pwhl_stats()` scoping bug and team ID resolution.
* Fixed `refresh_xg_models()` locked binding error by using package environment.
* Added complete test coverage for all exported functions (482 tests passing).
* Updated CI workflows and documentation.

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
