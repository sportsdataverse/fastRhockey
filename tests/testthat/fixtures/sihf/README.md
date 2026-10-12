# SIHF fixtures (fastRhockey)

Real responses from `https://data.sihf.ch/statistic/api/cms`, copied unchanged from sdv-py
`tests/fixtures/sihf_api_web/` (README_sdv_py.md is that directory's README: capture dates,
requests, which bodies are cut to their first rows). The table bodies are cut but still announce
the full row count in `pages`; the tests set `pages` to the rows kept before mocking the getter.

Written from sdv-py `origin/main` (79dd939371, after #826), with the sdv-py parsers run on these same
fixtures (never installed into any venv; a `git archive` of the package on PYTHONPATH):

| File | What |
|---|---|
| `columns.json` | the documented columns of each of the 33 routes (sdv-py `_PARSER_COLUMNS`) |
| `parity_cases.csv` | case -> route -> fixture (52 cases: every route, and the whole-table `full/` bodies) |
| `parity_dtypes.csv` | the polars dtype of every column of every case |
| `parity/<case>.csv` | the sdv-py frame: lists comma-joined (`NA` for a null element), null cell = `<NA>`, booleans `true`/`false` |

Do not hand-edit. To refresh after a sdv-py parser change, re-run the sdv-py parsers on the fixtures
and rewrite these four files together.
