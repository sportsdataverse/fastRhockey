# magnus parity oracle

Frames produced by sdv-py, used by `tests/testthat/test-magnus.R` to check that the R parsers return the same values.

- **sdv-py commit:** `79dd939371` (origin/main, merged #826).
- **Generated:** 2026-10-12.
- **How:** the sdv-py parsers were run on the SAME fixtures committed under `tests/testthat/fixtures/magnus/` and the resulting polars frame was written to CSV, one file per route and fixture.
- **Null convention:** a null cell is written empty and an empty string as `""`; the test reads with `na.strings = ""`.
A test asserts no CSV holds a `""` cell, so that read cannot hide an empty-string-vs-NA difference.
Regenerate when sdv-py's magnus parsers change; never hand-edit a CSV.
