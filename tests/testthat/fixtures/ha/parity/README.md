# ha parity oracle

Frames produced by sdv-py, used by `tests/testthat/test-ha.R` to check that the R parsers return the same values.

- **sdv-py commit:** `79dd939371` (origin/main, merged #826).
- **Generated:** 2026-10-12.
- **How:** the sdv-py parsers were run on the SAME fixtures committed under `tests/testthat/fixtures/ha/` and the resulting polars frame was written to CSV, one file per route and fixture.
- **Null convention:** null cell = `\\N` (`na.strings = "\\\\N"`).

Regenerate when sdv-py's ha parsers change; never hand-edit a CSV.
