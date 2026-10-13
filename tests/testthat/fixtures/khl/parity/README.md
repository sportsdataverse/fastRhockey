# khl parity oracle

Frames produced by sdv-py, used by `tests/testthat/test-khl.R` to check that the R parsers return the same values.

- **sdv-py commit:** `79dd939371` (origin/main, merged #826).
- **Generated:** 2026-10-12.
- **How:** the sdv-py parsers were run on the SAME fixtures committed under `tests/testthat/fixtures/khl/` and the resulting polars frame was written to CSV, one file per route and fixture.
- **Null convention:** null cell = `<NA>` (`na.strings = "<NA>"`); `_schema.csv` holds the polars dtypes.

Regenerate when sdv-py's khl parsers change; never hand-edit a CSV.
